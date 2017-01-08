package opmodes;

import android.graphics.Color;

import com.qualcomm.ftccommon.Device;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.DigitalChannelController;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.FourWheelPivotTurnDeadReckon;
import team25core.GamepadTask;
import team25core.LightSensorCriteria;
import team25core.PeriodicTimerTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;
import team25core.TwoWheelDirectDriveDeadReckon;

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

    protected enum NumberOfBeacons {
        ONE,
        TWO,
        DEFAULT,
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;
    protected NumberOfBeacons numberOfBeacons;

    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final int LEFT_COLOR_PORT = MochaCalibration.LEFT_COLOR_PORT;
    private final int RIGHT_COLOR_PORT = MochaCalibration.RIGHT_COLOR_PORT;
    private final double PIVOT_MULTIPLIER = MochaCalibration.PIVOT_MULTIPLIER;
    private final double TURN_SPEED = MochaCalibration.TURN_SPEED;
    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;
    private final double LINE_SPEED = MochaCalibration.LINE_SPEED;
    private final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private final double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;
    private final double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final VelocityVortexBeaconArms.ServoType SERVO_TYPE = VelocityVortexBeaconArms.ServoType.CONTINUOUS;
    private final FourWheelPivotTurnDeadReckon.TurningSide RIGHT_TURN  = FourWheelPivotTurnDeadReckon.TurningSide.RIGHT;

    private static int TURN_MULTIPLY = 0;

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
    // private ColorSensor colorLeft;
    private ColorSensor colorRight;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;

    private FourWheelDirectDriveDeadReckon positionForParticleFromCorner;
    private FourWheelDirectDriveDeadReckon positionForParticle;
    private FourWheelDirectDriveDeadReckon positionForBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;
    private FourWheelPivotTurnDeadReckon alignWithLine;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButton;
    private FourWheelDirectDriveDeadReckon moveToNextButton;
    private FourWheelDirectDriveDeadReckon moveFastToLine;

    private LightSensorCriteria whiteLineRightCriteria;
    private LightSensorCriteria whiteLineLeftCriteria;

    private VelocityVortexBeaconArms beaconArms;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamepadSelection(event);
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

        beacon = hardwareMap.servo.get("beacon");
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
        whiteLineLeftCriteria.setThreshold(0.65);

        deviceInterfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
        deviceInterfaceModule.setDigitalChannelMode(0, DigitalChannelController.Mode.OUTPUT);
        deviceInterfaceModule.setDigitalChannelState(0, false);

        // colorLeft = hardwareMap.colorSensor.get("colorLeft");
        colorRight = hardwareMap.colorSensor.get("colorRight");

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
        positionForParticle.addSegment(DeadReckon.SegmentType.STRAIGHT, 8, -0.3);

        positionForParticleFromCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticleFromCorner.addSegment(DeadReckon.SegmentType.STRAIGHT, 10, -MOVE_SPEED);

        positionForBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 3, MOVE_SPEED);
        positionForBeacon.addSegment(DeadReckon.SegmentType.TURN, 50, -TURN_SPEED * TURN_MULTIPLY);

        moveFastToLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveFastToLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 58, -MOVE_SPEED);
        moveFastToLine.addSegment(DeadReckon.SegmentType.TURN, 35, TURN_SPEED);

        targetingLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 18, -LINE_SPEED);

        alignWithLine = new FourWheelPivotTurnDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignWithLine.addSegment(DeadReckon.SegmentType.TURN, 50, -LINE_SPEED);
        alignWithLine.setMultiplierSide(PIVOT_MULTIPLIER, RIGHT_TURN);

        alignColorSensorWithButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.TURN, 5, 0.75 * TURN_SPEED);
        alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4.5, 0.25 * -MOVE_SPEED);

        moveToNextButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToNextButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.25 * -MOVE_SPEED);

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
            beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, moveToNextButton, beacon, SERVO_TYPE, false);
        } else {
            blueInit();
            beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, moveToNextButton, beacon, SERVO_TYPE, true);
        }

        if (startingPosition == StartingPosition.CORNER) {
            initialMove(positionForParticleFromCorner);
        } else {
            initialMove(positionForParticle);
        }
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
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
            case RIGHT_BUMPER_DOWN:
                numberOfBeacons = NumberOfBeacons.ONE;
                persistentTelemetryTask.addData("NUMBER OF BEACONS: ", "" + numberOfBeacons);
                break;
            case RIGHT_TRIGGER_DOWN:
                numberOfBeacons = NumberOfBeacons.TWO;
                persistentTelemetryTask.addData("NUMBER OF BEACONS: ", "" + numberOfBeacons);
                break;
        }
    }
    protected void blueInit() {
        TURN_MULTIPLY = 1;
        rightLight.enableLed(false);
    }

    protected void redInit() {
        TURN_MULTIPLY = -1;
        leftLight.enableLed(false);
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

                        addTask(new SingleShotTimerTask(this.robot, 1500) {
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
                if (paddleCount >= 8) {
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
        addTask(new DeadReckonTask(this, positionForBeacon) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                RobotLog.i("163 Shooter is done, positioning to move to beacon #1");

                switch (event.kind) {
                    case PATH_DONE:
                        handlePositionedForBeacon();
                        break;
                    default:
                        break;
                }
            }
        });
    }

    protected void handlePositionedForBeacon()
    {
        RobotLog.i("163 Robot positioned for the beacon");
        addTask(new DeadReckonTask(this, moveFastToLine) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot has moved near the beacon");
                    handleMovedToBeacon();
                }
            }
        });
    }

    protected void handleMovedToBeacon()
    {

        RobotLog.i("163 Robot moved near the beacon");
        addTask(new DeadReckonTask(this, targetingLine, whiteLineLeftCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                RobotLog.i("163 Targeting the white line with leftLight");

                if (event.kind == EventKind.SENSOR_SATISFIED) {
                    RobotLog.i("163 Robot found the first part of the white line");
                    handleFoundOnePartOfWhiteLine(event);
                } else if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot moved past the white line");

                } else {
                    RobotLog.e("163 Unknown event occurred");
                }
            }
        });
    }

    /*
    protected void handleRobotSemiAligned()
    {
        RobotLog.i("163 Turning to compensate for shifting center-point");
        addTask(new DeadReckonTask(this, compensationMovement) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot is ready to turn");
                    handleFoundOnePartOfWhiteLine(event);
                }
            }
        });
    }
    */

    protected void handleFoundOnePartOfWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        rightLight.enableLed(true);

        switch (e.kind) {
            case SENSOR_SATISFIED:
                RobotLog.i("163 Turning to fully realign with the beacon");
                addTask(new DeadReckonTask(this, alignWithLine, whiteLineRightCriteria) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;
                        if (event.kind == EventKind.SENSOR_SATISFIED) {
                            RobotLog.i("163 Robot has fully realigned");
                            handleAlignedWithWhiteLine(event);
                        } else if (event.kind == EventKind.PATH_DONE) {
                            RobotLog.e("163 Robot rotated through the white line");
                        }
                    }
                });
                break;
            default:
                RobotLog.i("163 Unknown event occurred");
                break;
        }
    }

    protected void handleAlignedWithWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        switch (e.kind) {
            case SENSOR_SATISFIED:
                RobotLog.i("163 Moving backwards to align the color sensor with the beacon");
                addTask(new DeadReckonTask(this, alignColorSensorWithButton) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;

                        if (event.kind == EventKind.PATH_DONE) {
                            RobotLog.i("163 Color sensor has aligned with the beacon");
                            handleAlignedWithColor(event);
                        } else {
                            RobotLog.e("163 Unknown event occurred");
                        }
                     }
                });
                break;
        }
    }

    protected void handleAlignedWithColor(DeadReckonTask.DeadReckonEvent e)
    {
        RobotLog.i("163 Starting to look for color");

        int channelToUse;
        ColorSensor sensorToUse;
        /*
        if (alliance == Alliance.RED) {
            channelToUse = LEFT_COLOR_PORT;
            // sensorToUse = colorLeft;
        } else {
        */
            channelToUse = RIGHT_COLOR_PORT;
            sensorToUse = colorRight;
        // }

        addTask(new ColorSensorTask(this, sensorToUse, deviceInterfaceModule, false, true, channelToUse) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent color = (ColorSensorEvent) e;

                if (alliance == Alliance.BLUE) {
                    switch (color.kind) {
                        case BLUE:
                            RobotLog.i("163 First attempt, sensed blue");
                            beaconArms.deploy(true, true);
                            break;
                        case RED:
                            RobotLog.i("163 First attempt, sensed red");
                            beaconArms.deploy(false, true);
                            break;
                        case PURPLE:
                            RobotLog.i("163 Color sensor is not working");
                            beaconArms.stowServo();
                            break;
                    }
                } else {
                    RobotLog.i("163 First attempt, sensed red");
                    switch (color.kind) {
                        case RED:
                            RobotLog.i("163 First attempt, sensed red");
                            beaconArms.deploy(true, true);
                            break;
                        case BLUE:
                            RobotLog.i("163 First attempt, sensed blue");
                            beaconArms.deploy(false, true);
                            break;
                        case PURPLE:
                            RobotLog.i("163 Color sensor is not working");
                            beaconArms.stowServo();
                            break;
                    }
                }
            }
        });
    }
}
