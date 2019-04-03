package test;
import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.hardware.rev.RevBlinkinLedDriver;
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
import team25core.IMUGyroTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.VuforiaConstants;

/**
 * Created by Lizzie on 3/31/2019.
 */
public class TwainTest extends Robot {

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
    private SwitchableCamera switchableCamera;
    private VuforiaLocalizer vuforia;
    private WebcamName webCam1;
    private WebcamName webCam2;

    // declaring telemetry items
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;

    // declaring DeadReckonPaths
    private DeadReckonPath knockLeftPath;
    private DeadReckonPath knockCenterPath;
    private DeadReckonPath knockRightPath;
    private DeadReckonPath knockPath;
    private DeadReckonPath liftBufferPath;
    private DeadReckonPath alignPath;

    // declaring dropoff util
    private HoustonDropoffUtil dropoff;
    public static HoustonDropoffUtil.MineralPosition goldMineralPosition;
    public static HoustonDropoffUtil.StartingPosition robotStartingPosition;
    public static HoustonDropoffUtil.EndingPosition robotEndingPosition;
    public static HoustonDropoffUtil.HangingPosition robotHangingPosition;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;

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

        // initializing cameras and vuforia
        this.switchableCamera = (SwitchableCamera)vuforia.getCamera();
        webCam1 = hardwareMap.get(WebcamName.class, "mineralCamera1");
        webCam2 = hardwareMap.get(WebcamName.class, "mineralCamera2");

        SwitchableCameraName switchableCameraName = ClassFactory.getInstance().getCameraManager().nameForSwitchableCamera(webCam1, webCam2);
        int cameraMonitorViewId = hardwareMap.appContext.getResources().getIdentifier("cameraMonitorViewId", "id", hardwareMap.appContext.getPackageName());
        VuforiaLocalizer.Parameters parameters = new VuforiaLocalizer.Parameters(cameraMonitorViewId);
        parameters.vuforiaLicenseKey = VuforiaConstants.VUFORIA_KEY;

        parameters.cameraName = switchableCameraName;
        this.vuforia = ClassFactory.getInstance().createVuforia(parameters);

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT SELECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");

        // initializing DeadReckonPaths
        knockLeftPath = new DeadReckonPath();
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 10.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 30.0, 0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8.0, 0.3);

        knockCenterPath = new DeadReckonPath();
        knockCenterPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.3);

        knockRightPath = new DeadReckonPath();
        knockRightPath.addSegment(DeadReckonPath.SegmentType.TURN, 30, 0.3);
        knockRightPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.5);

        knockPath = new DeadReckonPath();

        // initializing dropoff util
        dropoff = new HoustonDropoffUtil();

        alignPath = new DeadReckonPath();
        alignPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.5);

        // initializing lift variables
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        // initializing mineral detection and gyro event
        initializeMineralDetection();
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
                        knockPath = knockLeftPath;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_LEFT_PATTERN);
                        break;
                    case RIGHT:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.RIGHT;
                        knockPath = knockRightPath;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_RIGHT_PATTERN);
                        break;
                    case CENTER:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                        knockPath = knockCenterPath;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_CENTER_PATTERN);
                        break;
                    case UNKNOWN:
                        goldMineralPosition = HoustonDropoffUtil.MineralPosition.CENTER;
                        blinkin.setPattern(VivaldiCalibration.MINERAL_UNKNOWN_PATTERN);
                        knockPath = knockCenterPath;
                        break;
                }
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }

    @Override
    public void start() {
        liftUp();
    }

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
                    doneAligning(alignPath);
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
                       initialMove(knockPath);
                }
            }
        });
    }

    public void initialMove(DeadReckonPath path) {
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
        toggleCamera();
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent (RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                if (updatedMinerals != null) {
                    if (updatedMinerals.size() == 1) {
                        int goldMineral = -1;
                        for (Recognition recognition : updatedMinerals) {
                            if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                                goldMineral = (int) recognition.getLeft();
                                int imageCenter = recognition.getImageWidth() / 2;
                                float mineralCenter = recognition.getWidth() + recognition.getLeft();
                                float offset = java.lang.Math.abs(imageCenter - mineralCenter);

                                if(mineralCenter < imageCenter) {
                                    shiftRight(offset);
                                    // gonna sleep and continue tmmrw
                                } else if (imageCenter < mineralCenter) {
                                    shiftLeft(offset);
                                }
                            }
                        }
                    }
                }
            }
        };
    }

    public void shiftLeft(float correction) {
        float turnSlope = correction * VivaldiCalibration.GYRO_MULTIPLIER;
    }

    public void shiftRight(float correction) {

    }

    public void toggleCamera() {
        if (switchableCamera != null) {
            if (switchableCamera.getActiveCamera().equals(webCam1)) {
                switchableCamera.setActiveCamera(webCam2);
                RobotLog.i("163: Switching to camera 2.");
            } else {
                switchableCamera.setActiveCamera(webCam1);
                RobotLog.i("4042: Switching to camera 1.");
            }
        }
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // nothing to see here...keep scrolling
    }
}
