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
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/19/2016.
 */
@Autonomous(name = "Particle Beacon", group = "5218")
public class MochaParticleBeaconAutonomous extends Robot
{
    protected enum Alliance {
        BLUE,
        RED
    }

    protected enum StartingPosition {
        CORNER,
        VORTEX
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;

    private static int TURN_MULTIPLY = 0;
    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private final double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;
    private final double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;

    private int paddleCount;

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

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;

    private FourWheelDirectDriveDeadReckon positionForParticleFromCorner;
    private FourWheelDirectDriveDeadReckon positionForParticle;
    private FourWheelDirectDriveDeadReckon moveToBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;

    private LightSensorCriteria whiteLineRightCriteria;
    private LightSensorCriteria whiteLineLeftCriteria;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;

            switch (event.kind) {
                case BUTTON_X_DOWN:
                    alliance = Alliance.BLUE;
                    persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                    break;
                case BUTTON_B_DOWN:
                    alliance = Alliance.RED;
                    persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                    break;
                case BUTTON_Y_DOWN:
                    startingPosition = StartingPosition.CORNER;
                    persistentTelemetryTask.addData("POSITION: ", "" + startingPosition);
                    break;
                case BUTTON_A_DOWN:
                    startingPosition = StartingPosition.VORTEX;
                    persistentTelemetryTask.addData("POSITION: ", "" + startingPosition);
                    break;
            }
        } else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            handlePaddleEncoderDoneEvent(event);
        }
    }

    @Override
    public void init()
    {
        alliance = Alliance.BLUE;
        startingPosition = StartingPosition.VORTEX;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        persistentTelemetryTask.addData("ALLIANCE: ", "NOT SELECTED");
        persistentTelemetryTask.addData("POSITION: ", "NOT SELECTED");

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
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        paddleCount = 0;

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 50, 0.8);

        positionForParticle = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticle.addSegment(DeadReckon.SegmentType.STRAIGHT, 6, -0.65);

        positionForParticleFromCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticleFromCorner.addSegment(DeadReckon.SegmentType.STRAIGHT, 10, -0.65);

        moveToBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 6, 0.45);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, -0.3 * TURN_MULTIPLY);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 57 , -0.65);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, 0.3 * TURN_MULTIPLY);

        targetingLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 20, -0.25);

        whiteLineRightCriteria = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        whiteLineRightCriteria.setThreshold(0.65);
        whiteLineLeftCriteria = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
    }

    protected void startShooterCorner()
    {
        shooterLeft.setPower(SHOOTER_CORNER);
        shooterRight.setPower(-SHOOTER_CORNER);
    }

    protected void startShooterVortex()
    {
        shooterLeft.setPower(SHOOTER_VORTEX);
        shooterRight.setPower(-SHOOTER_VORTEX);
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
            redInit();
        } else {
            blueInit();
        }
        initialMove();
    }

    public void blueInit() {
        TURN_MULTIPLY = 1;
        leftLight.enableLed(false);
    }

    public void redInit() {
        TURN_MULTIPLY = -1;
        rightLight.enableLed(false);
    }

    protected void initialMove() {
        addTask(new DeadReckonTask(this, positionForParticle) {
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent)e;
                switch(event.kind) {
                    case PATH_DONE:
                        if (startingPosition == StartingPosition.CORNER) {
                            startShooterCorner();
                        } else {
                            startShooterVortex();
                        }

                        addTask(new SingleShotTimerTask(this.robot, 2000) {
                            @Override
                            public void handleEvent(RobotEvent e)
                            {
                                addTask(scoreCenterEncoderTask);
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

    protected void handlePaddleEncoderDoneEvent(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        switch (e.kind) {
            case DONE:
                if (paddleCount >= 15) {
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
                    RobotLog.i("163 Paddle count expired, iteration %d", paddleCount);
                    addTask(scoreCenterEncoderTask);
                    paddleCount++;
                }
            }
    }

    protected void handleMovedToBeaconEvent(DeadReckonTask.DeadReckonEvent e)
    {
        RobotLog.i("163 Robot moved to beacon");

        addTask(new DeadReckonTask(this, targetingLine, whiteLineRightCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                RobotLog.i("163 Targeting the white line");

                DeadReckonEvent event = (DeadReckonEvent) e;
                handleFoundWhiteLine(event);
            }
        });
    }

    protected void handleFoundWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind) {
            case SENSOR_SATISFIED:
                RobotLog.i("163 Robot finished moving to the white line");
                break;
            case PATH_DONE:
                RobotLog.i("163 Robot moved past the white line");
                break;
        }
    }
}
