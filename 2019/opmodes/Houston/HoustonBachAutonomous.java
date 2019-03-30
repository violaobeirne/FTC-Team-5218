package opmodes.Houston;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.hardware.rev.RevBlinkinLedDriver;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import opmodes.Houston.HoustonDropoffUtil;
import opmodes.Houston.HoustonExitDepotDropoff;
import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMUGyroTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "Houston Master Autonomous")
public class HoustonBachAutonomous extends Robot {

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;
    private Servo marker;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;
    private Telemetry.Item gyroItem;
    private IMUGyroTask gyroTask;
    private DeadReckonPath liftBufferPath;
    private DeadReckonTask liftBufferTask;

    // imu and lights
    BNO055IMU imu;
    RevBlinkinLedDriver blinkin;

    // knock paths and utilities
    private DeadReckonPath knockPath;
    private HoustonDropoffUtil dropoff;
    private DeadReckonPath exitDepotPath;
    private HoustonExitDepotDropoff exitDepotDropoff;

    // declaring gamepad variables
    private GamepadTask gamepad1;
    private GamepadTask gamepad2;
    public static HoustonDropoffUtil.MineralPosition goldMineralPosition;
    public static HoustonDropoffUtil.StartingPosition robotStartingPosition;
    public static HoustonDropoffUtil.EndingPosition robotEndingPosition;
    public static HoustonDropoffUtil.HangingPosition robotHangingPosition;


    // declaring telemetry item
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item startingPositionItem;
    private Telemetry.Item hangingItem;
    private Telemetry.Item endingPositionItem;

    @Override
    public void init() {
        // drivetrain initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // lift initialization
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftBufferPath = new DeadReckonPath();
        liftBufferPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1, 0.3);
        liftBufferTask = new DeadReckonTask(this, liftBufferPath, drivetrain);

        /*
        // bungee box initialization
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        // bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);
        */

        // imu and lights
        blinkin = hardwareMap.get(RevBlinkinLedDriver.class, "blinkin");
        imu = hardwareMap.get(BNO055IMU.class, "IMU");

        // depot path
        knockPath = new DeadReckonPath();
        dropoff = new HoustonDropoffUtil();
        exitDepotPath = new DeadReckonPath();
        exitDepotDropoff = new HoustonExitDepotDropoff();

        // initializing gamepad variables
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);
        gamepad2 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad2);

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        startingPositionItem = telemetry.addData("Starting Position", "NOT SELECTED");
        hangingItem = telemetry.addData("Hanging: ", "NOT SELECTED");
        endingPositionItem = telemetry.addData("Ending Position: ", "NOT SELECTED");

        // initializing mineral detection, marker movement, and IMU
        initializeMineralDetection();
        gyroItem = telemetry.addData("Gyro state:", "Not at target");
        marker = hardwareMap.servo.get("marker");
        handleGyroEvent();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                numberOfMineralsItem.setValue(updatedMinerals.size());
                HoustonDropoffUtil.MineralPosition goldPos = HoustonDropoffUtil.determineGoldPosition(updatedMinerals);
                HoustonDropoffUtil.sendPositionTelemetry(goldPos, goldMineralPositionItem);

                switch (goldPos) {
                    case LEFT:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.LEFT;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_LEFT_PATTERN);
                        break;
                    case RIGHT:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.RIGHT;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_RIGHT_PATTERN);
                        break;
                    case CENTER:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_CENTER_PATTERN);
                        break;
                    case UNKNOWN:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_UNKNOWN_PATTERN);
                        break;
                }
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            // starting position, hanging, parking, gold mineral position, double sampling
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    endingPositionItem.setValue("PARKING");
                    robotEndingPosition = HoustonDropoffUtil.EndingPosition.PARKING;
                    break;
                case BUTTON_Y_DOWN:
                    endingPositionItem.setValue("NOT PARKING");
                    robotEndingPosition = HoustonDropoffUtil.EndingPosition.NOT_PARKING;
                    break;
                case RIGHT_BUMPER_DOWN:
                    startingPositionItem.setValue("DEPOT");
                    robotStartingPosition = HoustonDropoffUtil.StartingPosition.DEPOT;
                    break;
                case RIGHT_TRIGGER_DOWN:
                    startingPositionItem.setValue("CRATER");
                    robotStartingPosition = HoustonDropoffUtil.StartingPosition.CRATER;
                    break;
                case LEFT_BUMPER_DOWN:
                    hangingItem.setValue("HANGING");
                    robotHangingPosition = HoustonDropoffUtil.HangingPosition.HANGING;
                    break;
                case LEFT_TRIGGER_DOWN:
                    hangingItem.setValue("NOT HANGING");
                    robotHangingPosition = HoustonDropoffUtil.HangingPosition.NOT_HANGING;
                    break;
            }
        }
    }

    public void handleGyroEvent ()
    {
        gyroTask = new IMUGyroTask(this, imu, 0, true) {
            @Override
            public void handleEvent (RobotEvent event) {
                if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                    drivetrain.stop();
                    bufferMove (liftBufferPath);
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
    }

    @Override
    public void start() {
        if(robotHangingPosition == HoustonDropoffUtil.HangingPosition.HANGING) {
            blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_HANGING_PATTERN);
            unlatch();
            knockPath = dropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
        } else if (robotHangingPosition == HoustonDropoffUtil.HangingPosition.NOT_HANGING) {
            blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_LANDED_PATTERN);
            knockPath = dropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            initialMove(knockPath);
        }
    }

    public void unlatch()
    {
        RobotLog.i("251 - Going UP");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE L");
                    liftLeft.setPower(0.0);
                    leftLanded = true;
                    if (rightLanded = true) {
                        initialStraighten();
                    }
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE R");
                    liftRight.setPower(0.0);
                    rightLanded = true;
                    if (leftLanded == true) {
                        initialStraighten();
                    }
                }
            } };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void bufferMove (final DeadReckonPath path)
    {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        initialMove(knockPath);
                        blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_PARKING_PATTERN);
                        break;
                }
            }
        });
    }

    public void initialStraighten()
    {
        addTask(gyroTask);
        drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
        blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_LANDED_PATTERN);
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if (robotStartingPosition == HoustonDropoffUtil.StartingPosition.DEPOT) {
                            markerDrop();
                        } else {
                            exitDepot(exitDepotPath);
                        }
                        break;
                }
            }
        });
    }

    protected void markerDrop() {
        RobotLog.i("Carrying out marker drop method.");
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_MARKER_DROP_PATTERN);
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        RobotLog.i("Carrying out exit depot method.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("251: You're done, congrats!");
                        blinkin.setPattern(VivaldiCalibration.AUTONOMUOS_DONE_PATTERN);
                }
            }
        });
    }

}
