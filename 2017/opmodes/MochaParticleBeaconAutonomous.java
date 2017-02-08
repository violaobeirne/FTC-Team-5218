package opmodes;

import com.qualcomm.hardware.modernrobotics.ModernRoboticsI2cRangeSensor;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.DigitalChannelController;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import team25core.AlignWithWhiteLineTask;
import team25core.AutonomousEvent;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.FourWheelDirectDrivetrain;
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
public class MochaParticleBeaconAutonomous extends Robot {

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

    public enum NumberOfBeacons {
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
    private final double RANGE_DISTANCE_MINIMUM = MochaCalibration.RANGE_DISTANCE_MINIMUM;
    private final double BEACON_TICKS_PER_CM = MochaCalibration.BEACON_TICKS_PER_CM;

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
    private ModernRoboticsI2cRangeSensor rangeSensor;
    private DeviceInterfaceModule deviceInterfaceModule;
    private ColorSensor color;

    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;
    private double distanceFromWall;
    private double ticksToMove;

    private FourWheelDirectDriveDeadReckon positionForBeacon;
    private FourWheelDirectDriveDeadReckon targetingLine;
    private FourWheelDirectDriveDeadReckon alignColorSensorWithButton;
    private FourWheelDirectDriveDeadReckon moveToNextButton;
    private FourWheelDirectDriveDeadReckon moveFastToLine;
    private FourWheelDirectDriveDeadReckon moveFastToNextBeacon;

    private FourWheelDirectDrivetrain drivetrain;

    private LightSensorCriteria rightSeesWhite;
    private LightSensorCriteria leftSeesWhite;
    private LightSensorCriteria rightSeesBlack;
    private LightSensorCriteria leftSeesBlack;

    private VelocityVortexBeaconArms beaconArms;

    @Override
    public void handleEvent(RobotEvent e) {
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
        beacon.setPosition(MochaCalibration.BEACON_STOWED_POSITION);

        sbod = hardwareMap.dcMotor.get("brush");
        paddleCount = 0;
        isOnSecondBeacon = false;
        isBlueAlliance = false;

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 50, 0.8);

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(true);
        leftLight.enableLed(true);

        rightSeesWhite = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        rightSeesWhite.setThreshold(0.65);
        leftSeesWhite = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.WHITE, LIGHT_MIN, LIGHT_MAX);
        leftSeesWhite.setThreshold(0.65);
        rightSeesBlack = new LightSensorCriteria(rightLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);
        rightSeesBlack.setThreshold(0.65);
        leftSeesBlack = new LightSensorCriteria(leftLight, LightSensorCriteria.LightPolarity.BLACK, LIGHT_MIN, LIGHT_MAX);
        leftSeesBlack.setThreshold(0.65);

        deviceInterfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
        deviceInterfaceModule.setDigitalChannelMode(0, DigitalChannelController.Mode.OUTPUT);
        deviceInterfaceModule.setDigitalChannelState(0, false);

        rangeSensor = hardwareMap.get(ModernRoboticsI2cRangeSensor.class, "rangeSensor");
        color = hardwareMap.colorSensor.get("color");

        drivetrain = new FourWheelDirectDrivetrain(MochaCalibration.TICKS_PER_INCH, MochaCalibration.PIVOT_MULTIPLIER, frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

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

        RobotLog.i("163 ========================= START ========================= ");
        alignToBeacon(50);
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
    }

    protected void redInit() {
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = -1;
    }

    /*
     * TODO: Remove after validating time for particle after pressing beacons.
     */
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

                        // handlePositionedForBeacon();
                        break;
                    default:
                        break;
                }
            }
        });
    }

    protected void alignToBeacon(int inchesToDiscard)
    {
        addTask(new AlignWithWhiteLineTask(this, inchesToDiscard, drivetrain, leftSeesBlack, leftSeesWhite, rightSeesBlack, rightSeesWhite) {
            @Override
            public void handleEvent(RobotEvent e) {
                AlignWithWhiteLineEvent ev = (AlignWithWhiteLineEvent)e;
                if (ev.kind == EventKind.ALIGNED || ev.kind == EventKind.GOOD_ENOUGH) {
                    handleAlignedWithWhiteLine();
                } else {
                    RobotLog.e("Didn't find the white line, aborting");
                }
            }
        });

    }

    protected void handleAlignedWithWhiteLine()
    {
        RobotLog.i("163 Moving backwards to align the color sensor with the beacon");
        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Aligning light sensor");

        alignColorSensorWithButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 3, 0.25 * -MOVE_SPEED);

        addTask(new DeadReckonTask(this, alignColorSensorWithButton) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Color sensor has aligned with the beacon");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Aligned color sensor");

                    addTask(new SingleShotTimerTask(robot, 750) {
                        @Override
                        public void handleEvent(RobotEvent e)
                        {
                            SingleShotTimerEvent event = (SingleShotTimerEvent) e;
                            if (event.kind == EventKind.EXPIRED) {
                                getDistanceFromWall();
                            }
                        }
                    });
                } else {
                    RobotLog.e("163 Unknown event occurred");
                }
             }
        });
    }

    protected void getDistanceFromWall()
    {
        addTask(new PeriodicTimerTask(this, 40){
            @Override
            public void handleEvent(RobotEvent e) {
                distanceFromWall = rangeSensor.getDistance(DistanceUnit.CM);
                RobotLog.i("163 Range sensor distance %f", distanceFromWall);
                if (distanceFromWall != 255) {
                    this.stop();
                    checkForDistanceFromWallUsingMRRange();
                }
            }
        });
    }
    protected void checkForDistanceFromWallUsingMRRange()
    {
        RobotLog.i("163 Distance from wall: " + distanceFromWall);

        if (distanceFromWall > MochaCalibration.RANGE_DISTANCE_MINIMUM) {
            ticksToMove = (((distanceFromWall - MochaCalibration.RANGE_DISTANCE_MINIMUM) * MochaCalibration.BEACON_TICKS_PER_CM)/(float)256.0);
            RobotLog.i("163 Moving the servo to color read position " + (ticksToMove + MochaCalibration.BEACON_STOWED_POSITION));
            beacon.setPosition(ticksToMove + MochaCalibration.BEACON_STOWED_POSITION);
        }

        addTask(new SingleShotTimerTask(this, 3000) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                SingleShotTimerEvent event = (SingleShotTimerEvent) e;
                if (event.kind == EventKind.EXPIRED) {
                    handleAlignedWithColor();
                }
            }
        });
    }

    protected void handleAlignedWithColor()
    {
        moveToNextButton = new FourWheelDirectDriveDeadReckon
                (this, MochaCalibration.TICKS_PER_INCH, MochaCalibration.TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToNextButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.5 * -MochaCalibration.MOVE_SPEED);

        final double smartStowValue = ticksToMove;

        beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, moveToNextButton, beacon, isBlueAlliance, numberOfBeacons, smartStowValue);
        addTask(new ColorSensorTask(this, color, deviceInterfaceModule, false, true, 0) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent color = (ColorSensorEvent) e;

                if (isBlueAlliance) {
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

    protected void handleBeaconWorkDone(AutonomousEvent e)
    {
        if (!isOnSecondBeacon) {
            RobotLog.i("163 Beacon work for beacon one is done");
            if (e.kind == AutonomousEvent.EventKind.BEACON_DONE) {
                RobotLog.i("163 Autonomous event is of type BeaconDone");
                isOnSecondBeacon = true;
                alignToBeacon(42);
            }
        } else {
            RobotLog.i("163 Beacon work called after second beacon");
        }
    }


    /*
     * TODO: Remove after validating time for particle after pressing beacons.
     */
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
                    // handleReadyForBeacon(positionForBeacon);
                }
                break;
            default:
                break;
        }
    }

}
