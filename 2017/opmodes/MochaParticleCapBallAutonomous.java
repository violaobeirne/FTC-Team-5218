package opmodes;

/*
 * Created by izzielau on 12/17/2016.
 */

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
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

@Autonomous(name = "Particle Cap Ball", group = "AutoTest")
public class MochaParticleCapBallAutonomous extends Robot {

    protected enum Alliance {
        BLUE,
        RED,
        DEFAULT
    }

    protected enum StartingPosition {
        CORNER,
        VORTEX,
        DEFAULT
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


    private FourWheelDirectDriveDeadReckon moveAwayFromWallReckonFromCorner;
    private FourWheelDirectDriveDeadReckon moveAwayFromWallReckonFromVortex;

    private FourWheelDirectDriveDeadReckon pushCapDeadReckonFromCorner;
    private FourWheelDirectDriveDeadReckon pushCapDeadReckonFromVortex;

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
        }
        else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            handlePaddleEncoderDoneEvent(event);
        }
    }

    @Override
    public void init()
    {
        alliance = Alliance.DEFAULT;
        startingPosition = StartingPosition.DEFAULT;

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
        paddleCount = 0;

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 1, MochaCalibration.BRUSH_SPEED);

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(false);
        leftLight.enableLed(false);

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        moveAwayFromWallReckonFromCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveAwayFromWallReckonFromCorner.addSegment(DeadReckon.SegmentType.STRAIGHT, 1, -0.45);

        moveAwayFromWallReckonFromVortex = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveAwayFromWallReckonFromVortex.addSegment(DeadReckon.SegmentType.STRAIGHT, 7, -0.45);

        pushCapDeadReckonFromCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        pushCapDeadReckonFromCorner.addSegment(DeadReckon.SegmentType.STRAIGHT, 64, -0.75);

        pushCapDeadReckonFromVortex = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        pushCapDeadReckonFromVortex.addSegment(DeadReckon.SegmentType.STRAIGHT, 50, -0.75);
    }

    protected void startShooterCorner()
    {
        shooterLeft.setPower(SHOOTER_CORNER);
        shooterRight.setPower(-SHOOTER_CORNER);
    }

    protected void startShooterVortex()
    {
        shooterLeft.setPower(SHOOTER_VORTEX);
        shooterRight.setPower(SHOOTER_VORTEX);
    }

    protected void stopShooter()
    {
        shooterLeft.setPower(0);
        shooterRight.setPower(0);
    }

    @Override
    public void start()
    {
        if (alliance == Alliance.RED) {
            redInit();
        } else {
            blueInit();
        }
        if (startingPosition == StartingPosition.CORNER) {
            initialMove(moveAwayFromWallReckonFromCorner);
        } else {
            initialMove(moveAwayFromWallReckonFromVortex);
        }
    }

    public void blueInit() {
        TURN_MULTIPLY = 1;
    }

    public void redInit() {
        TURN_MULTIPLY = -1;
    }

    protected void initialMove(final DeadReckon path) {
        addTask(new DeadReckonTask(this, path) {
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

                        addTask(new SingleShotTimerTask(robot, 2000) {
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

    protected void handlePaddleEncoderDoneEvent(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        switch (e.kind) {
            case DONE:
                if (paddleCount >= 15) {
                    RobotLog.i("163 Stopping the shooter");
                    stopShooter();

                    if (startingPosition == StartingPosition.CORNER) {
                        handlePaddleCountFinished(pushCapDeadReckonFromCorner);
                    } else {
                        handlePaddleCountFinished(pushCapDeadReckonFromVortex);
                    }

                } else {
                    RobotLog.i("163 Paddle count expired, iteration %d", paddleCount);
                    addTask(scoreCenterEncoderTask);
                    paddleCount++;
                }
        }
    }

    protected void handlePaddleCountFinished(DeadReckon path)
    {
        RobotLog.i("163 Paddle count finished");

        addTask(new DeadReckonTask(this, path) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                RobotLog.i("163 Shooter is done, moving to cap ball");
            }
        });
    }
}
