package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.GamepadTask;
import team25core.LightSensorCriteria;
import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/19/2016.
 */
@Autonomous(name = "(S) Vortex (G) Particle Beacon", group = "5218")
public class MochaParticleBeaconVAutonomous extends Robot
{

    protected enum Alliance {
        BLUE,
        RED
    }

    protected Alliance alliance;

    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final int LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private final int LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private final static double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final static double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;

    private int turnMultiplier = 0;

    private DcMotorController mc;
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;
    private LightSensor rightLight;
    private LightSensor leftLight;

    private int paddleCount;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PeriodicTimerTask ptt;

    private FourWheelDirectDriveDeadReckon positionForParticle;
    private FourWheelDirectDriveDeadReckon moveToBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;

    private LightSensorCriteria whiteLineRightCriteria;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            // TODO: Combine autonomous programs
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    alliance = Alliance.BLUE;
                    telemetry.addData("Alliance: ", alliance);
                    break;
                case BUTTON_B_DOWN:
                    alliance = Alliance.RED;
                    telemetry.addData("Alliance: ", alliance);
                    break;
            }
        } else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            switch (event.kind) {
                case DONE:
                    if (paddleCount >= 10) {
                        RobotLog.i("163 Stopping the shooter");
                        stopShooter();

                        addTask(new DeadReckonTask(this, moveToBeacon) {
                            @Override
                            public void handleEvent(RobotEvent e) {
                                RobotLog.i("163 Shooter is done, moving to beacon one");

                                DeadReckonEvent event = (DeadReckonEvent) e;
                                handleMovedToBeaconEvent(event);
                            }
                        });
                    } else {
                        RobotLog.i("163 Paddle count expired, iteration %d");
                        addTask(scoreCenterEncoderTask);
                        paddleCount++;
                    }
            }
        }
    }

    @Override
    public void init()
    {
        paddleCount = 0;

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        sbod = hardwareMap.dcMotor.get("brush");

        mc = hardwareMap.dcMotorController.get("mechanisms");

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 50, 0.8);
        // ptt = new PeriodicTimerTask(this, 500);

        positionForParticle = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticle.addSegment(DeadReckon.SegmentType.STRAIGHT, 6, -0.65);

        moveToBeacon = new FourWheelDirectDriveDeadReckon(this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, -0.3 * turnMultiplier);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 47, -0.65);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, 0.3 * turnMultiplier);

        targetingLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 25, -0.65);

        whiteLineRightCriteria = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
    }

    protected void startShooter()
    {
        shooterLeft.setPower(0.45);
        shooterRight.setPower(-0.45);
    }

    protected void stopShooter()
    {
        shooterLeft.setPower(0);
        shooterRight.setPower(0);

        scoreCenterEncoderTask.stop();
    }

    @Override
    public void start()
    {
        if (alliance == Alliance.RED) {
            turnMultiplier = -1;
        } else {
            turnMultiplier = 1;
        }

        initialMove();
    }

    protected void initialMove() {
        addTask(new DeadReckonTask(this, positionForParticle) {
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent)e;
                switch(event.kind) {
                    case PATH_DONE:
                        startShooter();
                        addTask(new SingleShotTimerTask(robot, 5000) {
                            @Override
                            public void handleEvent(RobotEvent e)
                            {
                                robot.addTask(scoreCenterEncoderTask);
                            }
                        });
                        break;
                    default:
                        RobotLog.e("163 Unknown event kind");
                        break;
                }
            }
        });
    }

    protected void handleMovedToBeaconEvent(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind) {
            case PATH_DONE:
                addTask(new DeadReckonTask(this, targetingLine, whiteLineRightCriteria) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        RobotLog.i("163 Targeting the white line");

                        DeadReckonEvent event = (DeadReckonEvent) e;
                        handleFoundWhiteLine(event);
                    }
                });
                break;
            default:
                break;
        }
    }

    protected void handleFoundWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind) {
            case SENSOR_SATISFIED:
                RobotLog.i("163 Light sensor found the white line");
                // TODO: Do beacon stuff
                break;
            case PATH_DONE:
                RobotLog.i("163 Robot moved past the white line");
                break;
        }
    }
}
