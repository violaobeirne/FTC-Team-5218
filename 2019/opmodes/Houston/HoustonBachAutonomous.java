package opmodes.Houston;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.hardware.rev.RevBlinkinLedDriver;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.ClassFactory;
import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.SwitchableCamera;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;
import org.firstinspires.ftc.robotcore.internal.camera.delegating.SwitchableCameraName;

import java.util.List;

import opmodes.Houston.HoustonDropoffUtil;
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
import team25core.VuforiaConstants;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "Houston Master Autonomous")
public class HoustonBachAutonomous extends Robot {

    private final static String TAG = "HoustonBachAutonomous";

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;
    private DcMotor bungeeBox;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;
    private Telemetry.Item gyroItem;
    private IMUGyroTask gyroTask;

    // imu and lights
    BNO055IMU imu;
    RevBlinkinLedDriver blinkin;

    // knock paths and utilities
    private DeadReckonPath liftBufferPath;
    private DeadReckonPath exitLanderPath;
    private DeadReckonPath setupLeftPath;
    private DeadReckonPath setupCenterPath;
    private DeadReckonPath setupRightPath;
    private DeadReckonPath setupAlignPath;
    private DeadReckonPath approachingGoldPath;
    private DeadReckonPath knockGoldPath;
    private DeadReckonPath knockDepotPath;
    private DeadReckonPath exitDepotPath;
    private HoustonDropoffUtil dropoff;
    private HoustonExitDepotUtil exitDepotDropoff;

    // declaring gamepad variables
    private GamepadTask gamepad1;
    private GamepadTask gamepad2;
    public static HoustonDropoffUtil.MineralPosition goldMineralPosition;
    public static HoustonDropoffUtil.EndingPosition robotEndingPosition;
    public static HoustonDropoffUtil.HangingPosition robotHangingPosition;


    // declaring telemetry item
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item hangingItem;
    private Telemetry.Item endingPositionItem;

    // camera code
    private MineralDetectionTask mdTask;
    int goldMineralX;
    private boolean approaching = true;

    // enum for mineral detection states
    public enum MineralDetectionStates {
        DEFAULT,
        INIT,
        ALIGNING,
        DONE_ALIGNING,
    }
    MineralDetectionStates mineralDetectionState = MineralDetectionStates.DEFAULT;

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

        // bungee box initialization
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        // bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);

        // imu and lights
        blinkin = hardwareMap.get(RevBlinkinLedDriver.class, "blinkin");
        imu = hardwareMap.get(BNO055IMU.class, "IMU");

        // Deadreckon path and utilities
        liftBufferPath = new DeadReckonPath();
        liftBufferPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1, 0.3);

        exitLanderPath = new DeadReckonPath();
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);

        setupLeftPath = new DeadReckonPath();
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 20.0, 0.3);

        setupCenterPath = new DeadReckonPath();
        setupCenterPath.addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.3);
        setupCenterPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        setupCenterPath.addSegment(DeadReckonPath.SegmentType.TURN, 35.0, 0.3);

        setupRightPath = new DeadReckonPath();
        setupRightPath.addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.3);
        setupRightPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.5);
        setupRightPath.addSegment(DeadReckonPath.SegmentType.TURN, 45.0, 0.3);

        setupAlignPath = new DeadReckonPath();

        approachingGoldPath = new DeadReckonPath();
        approachingGoldPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, 0.5);

        knockGoldPath = new DeadReckonPath();
        knockGoldPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12.0, 0.5);

        knockDepotPath = new DeadReckonPath();
        dropoff = new HoustonDropoffUtil();
        exitDepotPath = new DeadReckonPath();
        exitDepotDropoff = new HoustonExitDepotUtil();

        // initializing gamepad variables
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);
        gamepad2 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad2);

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        hangingItem = telemetry.addData("Hanging: ", "NOT SELECTED");
        endingPositionItem = telemetry.addData("Ending Position: ", "NOT SELECTED");

        // initializing mineral detection, marker movement, and IMU
        mineralDetectionState = MineralDetectionStates.INIT;
        initializeMineralDetection();
        gyroItem = telemetry.addData("Gyro state:", "Not at target");
        handleGyroEvent();
    }

    protected void initializeMineralDetection() {
        mdTask = new MineralDetectionTask(this, "mineralCamera1", "mineralCamera2") {
            public void handleEvent (RobotEvent e) {
                switch(mineralDetectionState) {
                    case INIT:
                        MineralDetectionEvent event = (MineralDetectionEvent) e;
                        List<Recognition> updatedMinerals = event.minerals;
                        numberOfMineralsItem.setValue(updatedMinerals.size());
                        HoustonDropoffUtil.MineralPosition goldPos = HoustonDropoffUtil.determineGoldPosition(updatedMinerals);
                        HoustonDropoffUtil.sendPositionTelemetry(goldPos, goldMineralPositionItem);

                        switch (goldPos) {
                            case LEFT:
                                goldMineralPosition = HoustonDropoffUtil.MineralPosition.LEFT;
                                setupAlignPath = setupLeftPath;
                                blinkin.setPattern(VivaldiCalibration.MINERAL_LEFT_PATTERN);
                                break;
                            case RIGHT:
                                goldMineralPosition = HoustonDropoffUtil.MineralPosition.RIGHT;
                                setupAlignPath = setupRightPath;
                                blinkin.setPattern(VivaldiCalibration.MINERAL_RIGHT_PATTERN);
                                break;
                            case CENTER:
                                goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                                setupAlignPath = setupCenterPath;
                                blinkin.setPattern(VivaldiCalibration.MINERAL_CENTER_PATTERN);
                                break;
                            case UNKNOWN:
                                goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                                blinkin.setPattern(VivaldiCalibration.MINERAL_UNKNOWN_PATTERN);
                                setupAlignPath = setupCenterPath;
                                break;
                        }
                        break;

                    case ALIGNING:
                        event = (MineralDetectionEvent) e;
                        Recognition goldMineral;
                        List<Recognition> singletonMineralList = event.minerals;
                        goldMineral= singletonMineralList.get(0);

                        goldMineralX = (int) goldMineral.getLeft();
                        int imageCenter = goldMineral.getImageWidth() / 2;
                        int mineralCenter = ((int)goldMineral.getWidth() / 2) + goldMineralX;
                        int offset = java.lang.Math.abs(imageCenter - mineralCenter);
                        if (Math.abs(offset) < VivaldiCalibration.OFFSET_SLOP) {
                            mineralDetectionState = MineralDetectionStates.DONE_ALIGNING;
                        } else if (mineralCenter < imageCenter) {
                            RobotLog.ii(TAG, "163: Shifting left. %d/%d/%d", imageCenter, mineralCenter, offset);
                            drivetrain.turn(VivaldiCalibration.TURN_SPEED);
                            // shiftLeft(offset);
                        } else if (imageCenter < mineralCenter) {
                            RobotLog.ii(TAG, "163: Shifting right. %d/%d/%d", imageCenter, mineralCenter, offset);
                            drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
                            // shiftRight(offset);
                        }
                        // this.stop();
                        break;

                    case DONE_ALIGNING:
                        RobotLog.ii(TAG, "163: Done aligning case in initialize mineral detection.");
                        if (approaching == true) {
                            approachGold(approachingGoldPath);
                            approaching = false;
                        } else {
                            knockGold(knockGoldPath);
                        }
                        this.stop();
                        break;
                }
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        addTask(mdTask);
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
                    bufferMove(liftBufferPath);
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
    }

    @Override
    public void start() {
        mdTask.stop();
        if(robotHangingPosition == HoustonDropoffUtil.HangingPosition.HANGING) {
            blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_HANGING_PATTERN);
            unlatch();
            knockDepotPath = dropoff.getPath(robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotEndingPosition, goldMineralPosition);
        } else if (robotHangingPosition == HoustonDropoffUtil.HangingPosition.NOT_HANGING) {
            blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_LANDED_PATTERN);
            knockDepotPath = dropoff.getPath(robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotEndingPosition, goldMineralPosition);
            initialMove(exitLanderPath);
        }
    }

    public void unlatch()
    {
        RobotLog.ii(TAG, "251 - Unlatching Going UP");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.ii(TAG, "251 - DONE L");
                    liftLeft.setPower(0.0);
                    leftLanded = true;
                    if (rightLanded == true) {
                        initialStraighten();
                    }
                }
            }
        };

        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.ii(TAG, "251 - DONE R");
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

    public void bufferMove(final DeadReckonPath path)
    {
        RobotLog.ii(TAG, "Buffer move start");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.ii(TAG, "Buffer move done");
                        initialMove(exitLanderPath);
                        blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_PARKING_PATTERN);
                        break;
                }
            }
        });
    }

    public void initialStraighten()
    {
        RobotLog.ii(TAG, "Initial straighten");
        addTask(gyroTask);
        drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
        blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_LANDED_PATTERN);
    }

    protected void initialMove(final DeadReckonPath path) {
        RobotLog.ii(TAG, "Initial move start");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.ii(TAG, "Initial move done");
                        setupAlign(setupAlignPath);
                        break;
                }
            }
        });
    }

    public void setupAlign(DeadReckonPath path) {
        RobotLog.ii(TAG, "163: Center on mineral");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        initializeMineralAlignment();
                }
            }
        });
    }

    public void initializeMineralAlignment() {
        RobotLog.ii(TAG, "163: Initializing mineral alignment.");
        mdTask.toggleCamera();
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.LARGEST_GOLD);
        mineralDetectionState = MineralDetectionStates.ALIGNING;
        addTask(mdTask);
    }

    public void approachGold(DeadReckonPath path) {
        RobotLog.ii(TAG, "163: Approaching gold");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        mineralDetectionState = MineralDetectionStates.ALIGNING;
                        addTask(mdTask);
                }
            }
        });
    }

    public void knockGold(DeadReckonPath path) {
        RobotLog.ii(TAG, "163: Knock gold path.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        knockDepot(knockDepotPath);
                }
            }
        });
    }

    protected void knockDepot(final DeadReckonPath path) {
        RobotLog.ii(TAG, "Carrying out exit depot method.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        markerDrop();
                }
            }
        });
    }

    protected void markerDrop() {
        RobotLog.ii(TAG, "Carrying out marker drop method.");
        addTask(new RunToEncoderValueTask(this, bungeeBox, 50, 0.3) {
            @Override
            public void handleEvent(RobotEvent e) {
                blinkin.setPattern(VivaldiCalibration.AUTONOMOUS_MARKER_DROP_PATTERN);
                exitDepot(exitDepotPath);
                // retractArm();
            }
        });
    }

    protected void retractArm() {
        addTask(new RunToEncoderValueTask(this, bungeeBox, 50, -0.3) {
            @Override
            public void handleEvent(RobotEvent e) {
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        RobotLog.ii(TAG, "Exiting depot and parking.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: You're done, congrats!");
                        blinkin.setPattern(VivaldiCalibration.AUTONOMUOS_DONE_PATTERN);

                }
            }
        });
    }

}
