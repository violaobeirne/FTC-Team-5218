package test;
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
import team25core.VuforiaConstants;

/**
 * Created by Lizzie on 3/31/2019.
 */
@Autonomous(name = "Twain Test")
public class TwainTest extends Robot {
    // enum for mineral detection states
    public enum MineralDetectionStates {
        DEFAULT,
        INIT,
        ALIGNING,
        DONE_ALIGNING,
    }

    MineralDetectionStates mineralDetectionState = MineralDetectionStates.DEFAULT;

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    // imu and lights
    BNO055IMU imu;
    RevBlinkinLedDriver blinkin;

    // camera code
    private MineralDetectionTask mdTask;
    int goldMineralX;
    private boolean approaching = true;

    // declaring telemetry items
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;

    // declaring DeadReckonPaths
    private DeadReckonPath exitLanderPath;
    private DeadReckonPath setupLeftPath;
    private DeadReckonPath setupCenterPath;
    private DeadReckonPath setupRightPath;
    private DeadReckonPath setupAlignPath;
    private DeadReckonPath liftBufferPath;

    private DeadReckonPath approachingGoldPath;
    private DeadReckonPath knockGoldPath;

    // declaring dropoff util
    private HoustonDropoffUtil dropoff;
    public static HoustonDropoffUtil.MineralPosition goldMineralPosition;
    /*
    public static HoustonDropoffUtil.StartingPosition robotStartingPosition;
    public static HoustonDropoffUtil.EndingPosition robotEndingPosition;
    public static HoustonDropoffUtil.HangingPosition robotHangingPosition;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;
    */

    // gyro variables
    private IMUGyroTask gyroTask;

    @Override
    public void init() {
        // initializing drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.encodersOn();
        drivetrain.resetEncoders();
        drivetrain.setNoncanonicalMotorDirection();

        // initializing imu and lights
        imu = hardwareMap.get(BNO055IMU.class, "IMU");
        blinkin = hardwareMap.get(RevBlinkinLedDriver.class, "blinkin");

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT SELECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");

        // initializing DeadReckonPaths
        exitLanderPath = new DeadReckonPath();
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        exitLanderPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);

        setupLeftPath = new DeadReckonPath();
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        setupLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);

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
        approachingGoldPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20.0, 0.5);
        knockGoldPath = new DeadReckonPath();
        knockGoldPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, 0.5);

        // initializing dropoff util
        dropoff = new HoustonDropoffUtil();

        // lift error path
        liftBufferPath = new DeadReckonPath();
        liftBufferPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);

        /*
        // initializing lift variables
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        */

        // initializating mineral detection state and general gold mineral detection
        mineralDetectionState = MineralDetectionStates.INIT;
        initializeMineralDetection();
        // handleGyroEvent();
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
                        if (Math.abs(offset) < 20) {
                            mineralDetectionState = MineralDetectionStates.DONE_ALIGNING;
                        } else if (mineralCenter < imageCenter) {
                            RobotLog.i("163: Shifting left. %d/%d/%d", imageCenter, mineralCenter, offset);
                            drivetrain.turn(VivaldiCalibration.TURN_SPEED);
                            // shiftLeft(offset);
                        } else if (imageCenter < mineralCenter) {
                            RobotLog.i("163: Shifting right. %d/%d/%d", imageCenter, mineralCenter, offset);
                            drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
                            // shiftRight(offset);
                        }
                        // this.stop();
                        break;

                    case DONE_ALIGNING:
                        RobotLog.i("163: Done aligning case in initialize mineral detection.");
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
    public void start() {
        mdTask.stop();
        initialMove(exitLanderPath);
    }

    public void initialMove(DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        setupAlign(setupAlignPath);
                }
            }
        });
    }

    public void setupAlign(DeadReckonPath path) {
        RobotLog.i("163: Center on mineral");
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
        RobotLog.i("163: Initializing mineral alignment.");
        mdTask.toggleCamera();
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.LARGEST_GOLD);
        mineralDetectionState = MineralDetectionStates.ALIGNING;
        addTask(mdTask);
    }

    public void shiftLeft(int correction) {
        double degrees = correction * VivaldiCalibration.GYRO_MULTIPLIER;
        RobotLog.i("163: Shifting left: " + degrees + " degrees.");
        gyroTask = new IMUGyroTask(this, imu, (int)degrees, true) {
            @Override
            public void handleEvent (RobotEvent event) {
                if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                    RobotLog.i("163: Hit target shifting left.");
                    drivetrain.stop();
                    if (mineralDetectionState == MineralDetectionStates.DONE_ALIGNING) {
                        knockGold(knockGoldPath);
                    } else {
                        approachGold(approachingGoldPath);
                    }
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    RobotLog.i("163: Past target shifting left.");
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
        addTask(gyroTask);
    }

    public void shiftRight(int correction) {
        double degrees = correction * VivaldiCalibration.GYRO_MULTIPLIER;
        RobotLog.i("163: Shifting right: " + degrees + " degrees.");
        gyroTask = new IMUGyroTask(this, imu, (int)degrees, true) {
            @Override
            public void handleEvent (RobotEvent event) {
                if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                    RobotLog.i("163: Hit target shifting right.");
                    drivetrain.stop();
                    if (mineralDetectionState == MineralDetectionStates.DONE_ALIGNING) {
                        knockGold(knockGoldPath);
                    } else {
                        approachGold(approachingGoldPath);
                    }
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    RobotLog.i("163: Past target shifting right.");
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
        addTask(gyroTask);
    }


    public void approachGold(DeadReckonPath path) {
        RobotLog.i("163: Approaching gold");
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
        RobotLog.i("163: Knock gold path.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        blinkin.setPattern(RevBlinkinLedDriver.BlinkinPattern.RAINBOW_RAINBOW_PALETTE);
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {
        /*
        if (e instanceof GamepadTask.GamepadEvent) {
            // starting position, hanging, parking, gold mineral position, double sampling
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    RobotLog.i("163: Pattern LEFT.");
                    goldMineralPosition = HoustonDropoffUtil.MineralPosition.LEFT;
                    setupAlignPath = setupLeftPath;
                    blinkin.setPattern(VivaldiCalibration.MINERAL_LEFT_PATTERN);
                    break;
                case BUTTON_B_DOWN:
                    RobotLog.i("163: Pattern RIGHT.");
                    goldMineralPosition = HoustonDropoffUtil.MineralPosition.RIGHT;
                    setupAlignPath = setupRightPath;
                    blinkin.setPattern(VivaldiCalibration.MINERAL_RIGHT_PATTERN);
                    break;
                case BUTTON_A_DOWN:
                    RobotLog.i("163: Pattern CENTER.");
                    goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                    setupAlignPath = setupCenterPath;
                    blinkin.setPattern(VivaldiCalibration.MINERAL_CENTER_PATTERN);
                    break;
            }
        }
        */
    }


    /*
    public void liftUp()
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
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void initialStraighten() {
        addTask(gyroTask);
        drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
    }

    public void handleGyroEvent() {
        gyroTask = new IMUGyroTask(this, imu, 0, true) {
            @Override
            public void handleEvent (RobotEvent event) {
                if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                    drivetrain.stop();
                    doneAligning(liftBufferPath);
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
    }

    public void doneAligning(DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                       initialMove(setupAlignPath);
                }
            }
        });
    }
    */
}
