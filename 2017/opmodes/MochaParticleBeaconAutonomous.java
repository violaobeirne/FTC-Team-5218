package opmodes;

import android.graphics.Color;

import com.qualcomm.ftccommon.Device;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.ColorSensorTask;
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
        RED,
        DEFAULT,
    }

    protected enum StartingPosition {
        CORNER,
        VORTEX,
        DEFAULT,
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;

    private static int TURN_MULTIPLY = 0;
    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final int LEFT_COLOR_PORT = MochaCalibration.LEFT_COLOR_PORT;
    private final int RIGHT_COLOR_PORT = MochaCalibration.RIGHT_COLOR_PORT;
    private final double TURN_SPEED = MochaCalibration.TURN_SPEED;
    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;
    private final double LINE_SPEED = MochaCalibration.LINE_SPEED;
    private final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private final double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;
    private final double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;

    private final BeaconArms5218.ServoType SERVO_TYPE = BeaconArms5218.ServoType.CONTINUOUS;

    private int paddleCount;
    private boolean isBlue;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;
    private Servo beacon;
    private LightSensor rightLight;
    private LightSensor leftLight;
    private DeviceInterfaceModule deviceInterfaceModule;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private ColorSensorTask leftColorSensorTask;
    private ColorSensorTask rightColorSensorTask;
    private GamepadTask gamepad;

    private FourWheelDirectDriveDeadReckon positionForParticleFromCorner;
    private FourWheelDirectDriveDeadReckon positionForParticle;
    private FourWheelDirectDriveDeadReckon moveToBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;
    private FourWheelDirectDriveDeadReckon moveNextToBeacon;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButton;

    private LightSensorCriteria whiteLineRightCriteria;
    private LightSensorCriteria whiteLineLeftCriteria;

    private BeaconArms5218 beaconSensor;

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
            handlePaddleEncoderDone(event);
        }
    }

    @Override
    public void init()
    {
        // Assign globals to default states to prevent errors.
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

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 50, 0.8);

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        whiteLineRightCriteria = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        whiteLineRightCriteria.setThreshold(0.65);
        whiteLineLeftCriteria = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        positionForParticle = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticle.addSegment(DeadReckon.SegmentType.STRAIGHT, 8, -MOVE_SPEED);

        positionForParticleFromCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticleFromCorner.addSegment(DeadReckon.SegmentType.STRAIGHT, 10, -MOVE_SPEED);

        moveToBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 5, MOVE_SPEED);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, -TURN_SPEED * TURN_MULTIPLY);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 75 , -MOVE_SPEED);
        moveToBeacon.addSegment(DeadReckon.SegmentType.TURN, 45, TURN_SPEED * TURN_MULTIPLY);
        moveToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 4 , 0.5 * -MOVE_SPEED);

        targetingLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 20, LINE_SPEED);

        moveNextToBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveNextToBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 5, -MOVE_SPEED);

        alignColorSensorWithButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 2, 0.5 * -MOVE_SPEED);

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
            initialMove(positionForParticleFromCorner);
        } else {
            initialMove(positionForParticle);
        }
    }

    protected void blueInit() {
        TURN_MULTIPLY = 1;
        leftLight.enableLed(false);
    }

    protected void redInit() {
        TURN_MULTIPLY = -1;
        rightLight.enableLed(false);
    }

    protected void initialMove(final DeadReckon path)
    {
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

    protected void handlePaddleEncoderDone(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        switch (e.kind) {
            case DONE:
                if (paddleCount >= 15) {
                    RobotLog.i("163 Stopping the shooter");
                    stopShooter();

                    handlePaddleCountFinished();
                } else {
                    RobotLog.i("163 Paddle count expired, iteration %d", paddleCount);
                    addTask(scoreCenterEncoderTask);
                    paddleCount++;
                }
            }
    }

    protected void handlePaddleCountFinished()
    {
        RobotLog.i("163 Paddle count finished");
        addTask(new DeadReckonTask(this, moveToBeacon) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                RobotLog.i("163 Shooter is done, moving to beacon one");

                switch (event.kind) {
                    case PATH_DONE:
                        handleMovedToBeacon(event);
                        break;
                    default:
                        break;
                }
            }
        });
    }

    protected void handleMovedToBeacon(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind) {
            case PATH_DONE:
                RobotLog.i("163 Robot moved to beacon");

                addTask(new DeadReckonTask(this, targetingLine, whiteLineRightCriteria) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;
                        RobotLog.i("163 Targeting the white line");

                        if (event.kind == EventKind.SENSOR_SATISFIED) {
                            RobotLog.i("163 Robot finished moving to the white line");
                            handleFoundWhiteLine(event);
                        } else {
                            RobotLog.e("163 Robot moved past the white line");
                        }
                    }
                });
                break;
        }
    }

    protected void handleFoundWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind)
        {
            case SENSOR_SATISFIED:
                addTask(new DeadReckonTask(this, alignColorSensorWithButton) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;

                        switch (event.kind) {
                            case PATH_DONE:

                                break;
                        }
                     }
                });
                break;
        }
    }
}
