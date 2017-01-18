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
import com.qualcomm.robotcore.hardware.UltrasonicSensor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.AutonomousEvent;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
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

    protected enum ShootForParticle {
        YES,
        NO,
        DEFAULT,
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;
    protected NumberOfBeacons numberOfBeacons;
    protected ShootForParticle shootForParticle;

    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final double PIVOT_MULTIPLIER = MochaCalibration.PIVOT_MULTIPLIER;
    private final double TURN_SPEED = MochaCalibration.TURN_SPEED;
    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;
    private final double LINE_SPEED = MochaCalibration.LINE_SPEED;
    private final double LIGHT_MIN = MochaCalibration.LIGHT_MINIMUM;
    private final double LIGHT_MAX = MochaCalibration.LIGHT_MAXIMUM;
    private final double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;
    private final double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final double ULTRASONIC_DISTANCE_MINIMUM = MochaCalibration.ULTRASONIC_DISTANCE_MINIMUM;
    private final FourWheelPivotTurnDeadReckon.TurningSide RIGHT_TURN = FourWheelPivotTurnDeadReckon.TurningSide.RIGHT;
    private final FourWheelPivotTurnDeadReckon.TurningSide LEFT_TURN = FourWheelPivotTurnDeadReckon.TurningSide.LEFT;

    private static int MOVE_MULTIPLIER = 0;
    private static int TURN_MULTIPLIER = 0;

    private int paddleCount;
    private boolean isOnSecondBeacon;
    private boolean isBlueAlliance;
    private boolean hasParallelParked;

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
    private UltrasonicSensor rightSound;
    private DeviceInterfaceModule deviceInterfaceModule;
    private ColorSensor color;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;

    private FourWheelDirectDriveDeadReckon positionForParticleFromBeaconCorner;
    private FourWheelDirectDriveDeadReckon positionForParticle;
    private FourWheelDirectDriveDeadReckon positionForBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;
    private FourWheelDirectDriveDeadReckon targetingSecondLine;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButton;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButtonAgain;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButtonAfterParallelPark;
    private FourWheelDirectDriveDeadReckon moveToNextButton;
    private FourWheelDirectDriveDeadReckon moveFastToLine;
    private FourWheelDirectDriveDeadReckon moveFastToNextBeacon;
    private FourWheelDirectDriveDeadReckon parallelPark;
    private FourWheelDirectDriveDeadReckon parallelParkLookForLine;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheLeft;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheRight;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheLeftAfter;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheRightAfter;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheRightParallelPark;
    private FourWheelPivotTurnDeadReckon pivotBackwardsToTheLeftParallelPark;

    private FourWheelPivotTurnDeadReckon pivotForwardsToTheLeft;
    private FourWheelDirectDriveDeadReckon pivotForwardsToTheRight;

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
        } else if (e instanceof AutonomousEvent) {
            AutonomousEvent event = (AutonomousEvent) e;
            handleBeaconWorkDone(event);
        }
    }

    @Override
    public void init()
    {
        // Assign globals to default states to prevent errors.
        alliance = Alliance.DEFAULT;
        startingPosition = StartingPosition.DEFAULT;
        numberOfBeacons = NumberOfBeacons.DEFAULT;
        shootForParticle = ShootForParticle.DEFAULT;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        persistentTelemetryTask.addData("ALLIANCE", "NOT SELECTED");
        persistentTelemetryTask.addData("POSITION", "NOT SELECTED");
        persistentTelemetryTask.addData("NUMBER OF BEACONS", "NOT SELECTED");
        persistentTelemetryTask.addData("SHOOT FOR PARTICLE", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        beacon = hardwareMap.servo.get("beacon");
        sbod = hardwareMap.dcMotor.get("brush");
        paddleCount = 0;
        isOnSecondBeacon = false;
        isBlueAlliance = false;
        hasParallelParked = false;

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

        color = hardwareMap.colorSensor.get("color");
        rightSound = hardwareMap.ultrasonicSensor.get("rightSound");

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
        positionForParticle.addSegment(DeadReckon.SegmentType.STRAIGHT, 8, -0.3 * MOVE_MULTIPLIER);

        positionForParticleFromBeaconCorner = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForParticleFromBeaconCorner.addSegment(DeadReckon.SegmentType.TURN, 45, -TURN_SPEED * TURN_MULTIPLIER);

        positionForBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        positionForBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 6, -MOVE_SPEED * MOVE_MULTIPLIER);
        positionForBeacon.addSegment(DeadReckon.SegmentType.TURN, 50, -TURN_SPEED);

        moveFastToLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveFastToLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 56.5, -MOVE_SPEED * MOVE_MULTIPLIER);
        moveFastToLine.addSegment(DeadReckon.SegmentType.TURN, 35, TURN_SPEED);

        targetingSecondLine = new FourWheelDirectDriveDeadReckon
            (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingSecondLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 18, -LINE_SPEED * MOVE_MULTIPLIER);

        pivotBackwardsToTheLeftAfter = new FourWheelPivotTurnDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        pivotBackwardsToTheLeftAfter.addSegment(DeadReckon.SegmentType.TURN, 50, -LINE_SPEED * TURN_MULTIPLIER);
        pivotBackwardsToTheLeftAfter.setMultiplierSide(PIVOT_MULTIPLIER, RIGHT_TURN);

        pivotBackwardsToTheRightAfter = new FourWheelPivotTurnDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        pivotBackwardsToTheRightAfter.addSegment(DeadReckon.SegmentType.TURN, 50, -LINE_SPEED);
        pivotBackwardsToTheRightAfter.setMultiplierSide(PIVOT_MULTIPLIER, LEFT_TURN);

        pivotForwardsToTheLeft = new FourWheelPivotTurnDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        pivotForwardsToTheLeft.addSegment(DeadReckon.SegmentType.TURN, 50, LINE_SPEED);
        pivotForwardsToTheLeft.setMultiplierSide(PIVOT_MULTIPLIER, RIGHT_TURN);

        alignColorSensorWithButtonAgain = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButtonAgain.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.25 * -MOVE_SPEED * MOVE_MULTIPLIER);

        alignColorSensorWithButtonAfterParallelPark = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButtonAfterParallelPark.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.25 * -MOVE_SPEED * MOVE_MULTIPLIER);

        moveFastToNextBeacon = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveFastToNextBeacon.addSegment(DeadReckon.SegmentType.STRAIGHT, 42, -MOVE_SPEED * MOVE_MULTIPLIER);

        parallelParkLookForLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        parallelParkLookForLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 10, -LINE_SPEED);

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
            isBlueAlliance = false;
        } else {
            blueInit();
            isBlueAlliance = true;
        }

        handlePositionedForBeacon();
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                alliance = Alliance.BLUE;
                persistentTelemetryTask.addData("ALLIANCE", "" + alliance);
                break;
            case BUTTON_B_DOWN:
                alliance = Alliance.RED;
                persistentTelemetryTask.addData("ALLIANCE", "" + alliance);
                break;
            case BUTTON_Y_DOWN:
                startingPosition = StartingPosition.CORNER;
                persistentTelemetryTask.addData("POSITION", "" + startingPosition);
                break;
            case BUTTON_A_DOWN:
                startingPosition = StartingPosition.VORTEX;
                persistentTelemetryTask.addData("POSITION", "" + startingPosition);
                break;
            case RIGHT_BUMPER_DOWN:
                numberOfBeacons = NumberOfBeacons.ONE;
                persistentTelemetryTask.addData("NUMBER OF BEACONS", "" + numberOfBeacons);
                break;
            case RIGHT_TRIGGER_DOWN:
                numberOfBeacons = NumberOfBeacons.TWO;
                persistentTelemetryTask.addData("NUMBER OF BEACONS", "" + numberOfBeacons);
                break;
            case LEFT_BUMPER_DOWN:
                shootForParticle = ShootForParticle.YES;
                persistentTelemetryTask.addData("SHOOT FOR PARTICLE", "" + shootForParticle);
                break;
            case LEFT_TRIGGER_DOWN:
                shootForParticle = ShootForParticle.NO;
                persistentTelemetryTask.addData("SHOOT FOR PARTICLE", "" + shootForParticle);
                break;
        }
    }

    protected void blueInit() {
        MOVE_MULTIPLIER = 1;
        TURN_MULTIPLIER = 1;
        rightLight.enableLed(false);
    }

    protected void redInit() {
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = -1;
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
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Unknown event kind");

                        break;
                }
            }
        });
    }

    protected void handlePaddleEncoderDone(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        switch (e.kind) {
            case DONE:
                if (paddleCount <= 8) {
                    RobotLog.i("163 Paddle count expired, iteration %d", paddleCount);
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Paddle count expired, iteration " + paddleCount);

                    addTask(scoreCenterEncoderTask);
                    paddleCount++;
                } else {
                    RobotLog.i("163 Paddle count finished, stopping the shooter");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Paddle count done, stopping shooter");

                    stopShooter();
                    handleReadyForBeacon(positionForBeacon);
                }
            }
    }

    protected void handleReadyForBeacon(DeadReckon path)
    {
        RobotLog.i("Positioning for beacon");
        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Positioning for beacon");

        addTask(new DeadReckonTask(this, path) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163 Robot positioned for the beacon");
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Positioned for beacon");

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
        RobotLog.i("163 Robot is moving fast to the white line");
        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Moving fast to white line");

        addTask(new DeadReckonTask(this, moveFastToLine) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot has moved near the beacon");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Moved near beacon");

                    handleMovedToBeacon();
                }
            }
        });
    }

    protected void handleMovedToBeacon()
    {
        RobotLog.i("163 Robot moved near the beacon");
        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Moved near beacon");

        LightSensorCriteria criteriaToUse;
        if (isBlueAlliance) {
            criteriaToUse = whiteLineLeftCriteria;
        } else {
            criteriaToUse = whiteLineRightCriteria;
        }

        targetingLine = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        targetingLine.addSegment(DeadReckon.SegmentType.STRAIGHT, 18, -LINE_SPEED * MOVE_MULTIPLIER);

        addTask(new DeadReckonTask(this, targetingLine, whiteLineLeftCriteria, whiteLineRightCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                RobotLog.i("163 Targeting the white line with leftLight");
                persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Targeting white line with left light");

                if ((event.kind == EventKind.LEFT_SENSOR_SATISFIED) || (event.kind == EventKind.RIGHT_SENSOR_SATISFIED)) {
                    RobotLog.i("163 Robot found the first part of the first line");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Sensor satisfied");

                    handleFoundOnePartOfWhiteLine(event);
                } else if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot moved past the white line");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Over-rotated");
                } else {
                    RobotLog.e("163 Unknown event occurred");
                }
            }
        });
    }

    protected void handleFoundOnePartOfWhiteLine(DeadReckonTask.DeadReckonEvent e)
    {
        final DeadReckonTask.DeadReckonEvent cachedEvent = e;
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        LightSensorCriteria criteriaToUse;
        FourWheelPivotTurnDeadReckon pivotReckonToUse;

        pivotReckonToUse = new FourWheelPivotTurnDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);

        switch (e.kind) {
            case LEFT_SENSOR_SATISFIED:
                RobotLog.i("163 Turning to fully realign with the beacon");
                persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Realigning fully");
                pivotReckonToUse.addSegment(DeadReckon.SegmentType.TURN, 50, -LINE_SPEED);
                pivotReckonToUse.setMultiplierSide(PIVOT_MULTIPLIER, RIGHT_TURN);
                criteriaToUse = whiteLineRightCriteria;
                break;
            case RIGHT_SENSOR_SATISFIED:
                pivotReckonToUse.addSegment(DeadReckon.SegmentType.TURN, 50, -LINE_SPEED);
                pivotReckonToUse.setMultiplierSide(PIVOT_MULTIPLIER, LEFT_TURN);
                criteriaToUse = whiteLineLeftCriteria;
                break;
            default:
                RobotLog.i("163 Unknown event occurred");
                return;
        }
        addTask(new DeadReckonTask(this, pivotReckonToUse, criteriaToUse) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.SENSOR_SATISFIED) {
                    RobotLog.i("163 Robot found the second part of the first line");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Sensor satisfied");
                    handleAlignedWithWhiteLine(event, cachedEvent);
                } else if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.e("163 Robot rotated through the white line");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Over-rotated");
                }
            }
        });
    }

    protected void handleAlignedWithWhiteLine(DeadReckonTask.DeadReckonEvent e, DeadReckonTask.DeadReckonEvent cachedEvent)
    {
        switch (e.kind) {
            case SENSOR_SATISFIED:
                RobotLog.i("163 Moving backwards to align the color sensor with the beacon");
                persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Aligning color sensor");

                alignColorSensorWithButton = new FourWheelDirectDriveDeadReckon
                        (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
                if (cachedEvent.kind == DeadReckonTask.EventKind.RIGHT_SENSOR_SATISFIED) {
                    RobotLog.i("163 Attempting to turn right through the white line");
                    alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.TURN, 5, -0.75 * TURN_SPEED);
                } else {
                    RobotLog.i("163 Attempting to turn left through the white line");
                    alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.TURN, 5, 0.75 * TURN_SPEED);
                }
                alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.25 * -MOVE_SPEED * MOVE_MULTIPLIER);

                addTask(new DeadReckonTask(this, alignColorSensorWithButton) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;

                        if (event.kind == EventKind.PATH_DONE) {
                            RobotLog.i("163 Color sensor has aligned with the beacon");
                            persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Aligned color sensor");

                            checkForDistanceFromWall();
                        } else {
                            RobotLog.e("163 Unknown event occurred");
                        }
                     }
                });
                break;
            case RIGHT_SENSOR_SATISFIED:
                RobotLog.i("163 Moving to the left");
        }
    }

    protected void checkForDistanceFromWall()
    {
        UltrasonicSensor sonicToUse;
        // if (isBlueAlliance) {
        sonicToUse = rightSound;
        /*} else {
            sonicToUse = leftSound;
        }
        */

        double ultrasonicValue = sonicToUse.getUltrasonicLevel();
        RobotLog.i("163 Starting to look for color, distance from wall: " + ultrasonicValue);

        if (ultrasonicValue <= ULTRASONIC_DISTANCE_MINIMUM) {
            if (isBlueAlliance) {
                beacon.setPosition(MochaCalibration.RIGHT_PRESSER_DIRECTION);
            } else {
                beacon.setPosition(MochaCalibration.LEFT_PRESSER_DIRECTION);
            }
            addTask(new SingleShotTimerTask(this, 500) {
                @Override
                public void handleEvent(RobotEvent e)
                {
                    SingleShotTimerEvent event = (SingleShotTimerEvent) e;
                    if (event.kind == EventKind.EXPIRED) {
                        beacon.setPosition(0.5);
                        handleAlignedWithColor();
                    }
                }
            });
        } else {
            handleParkNeedToRealign();
        }
    }

    protected void handleParkNeedToRealign()
    {
        parallelPark = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        parallelPark.addSegment(DeadReckon.SegmentType.TURN, 20, TURN_SPEED);
        parallelPark.addSegment(DeadReckon.SegmentType.STRAIGHT, 9, MOVE_SPEED);
        parallelPark.addSegment(DeadReckon.SegmentType.TURN, 25, -TURN_SPEED);

        addTask(new DeadReckonTask(this, parallelPark) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.PATH_DONE) {
                    // handleParkReadyToAlignWithLine();
                    handleMovedToBeacon();
                }
            }
        });
    }

    protected void handleAlignedWithColor()
    {
        moveToNextButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToNextButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.25 * -MOVE_SPEED * MOVE_MULTIPLIER);


        beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, moveToNextButton, beacon, isBlueAlliance, numberOfBeacons);
        addTask(new ColorSensorTask(this, color, deviceInterfaceModule, false, true, 0) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent color = (ColorSensorEvent) e;

                if (isBlueAlliance) {
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Color: " + color.kind);

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
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Color: " + color.kind);

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

    public void handleBeaconWorkDone(AutonomousEvent e)
    {
        if (!isOnSecondBeacon) {
            RobotLog.i("163 Beacon work for beacon one is done");
            if (e.kind == AutonomousEvent.EventKind.BEACON_DONE) {
                RobotLog.i("163 Autonomous event is of type BeaconDone");
                isOnSecondBeacon = true;
                addTask(new DeadReckonTask(this, moveFastToNextBeacon) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        DeadReckonEvent event = (DeadReckonEvent) e;
                        switch (event.kind) {
                            case PATH_DONE:
                                handleMovedToBeacon();
                                break;
                        }
                    }
                });
            }
        } else {
            initialMove(positionForParticleFromBeaconCorner);
        }
    }
}
