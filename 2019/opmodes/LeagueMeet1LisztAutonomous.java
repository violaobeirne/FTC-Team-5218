package opmodes;
import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.Camera;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMULevelSensorCriteria;
import team25core.MineralDetectionTask;
import team25core.OneWheelDirectDrivetrain;
import team25core .Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "League Meet 1 Mineral Autonomous")
public class LeagueMeet1LisztAutonomous extends Robot {
    private enum AllianceColor {
        BLUE,
        RED,
        DEFAULT
    }

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor lift;
    private Servo marker;
    private FourWheelDirectDrivetrain drivetrain;
    private OneWheelDirectDrivetrain caribinerDrivetrain;
    private DeadReckonPath lowerPath;
    private DeadReckonTask lowerTask;
    private BNO055IMU imu;

    // declaring other tasks and paths
    private RunToEncoderValueTask landingEncoderTask;
    private MarkerDropoff2 dropoff;
    private DeadReckonPath exitDepotPath;
    private DeadReckonPath knockPath;

    // declaring gamepad variables
    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected MarkerDropoff2.DropMarker dropMarker;
    protected MarkerDropoff2.GoldMineralPosition goldMineralPosition;

    // declaring telemetry item
    private Telemetry.Item allianceItem;
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item dropMarkerItem;
    private Telemetry.Item landedItem;

    @Override
    public void init() {
        // initializing motors, servos, and drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        lift = hardwareMap.dcMotor.get("lift");
        marker = hardwareMap.servo.get("marker");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        imu = hardwareMap.get(BNO055IMU.class, "imu");

        caribinerDrivetrain = new OneWheelDirectDrivetrain(lift);
        caribinerDrivetrain.resetEncoders();
        caribinerDrivetrain.encodersOn();
        lowerPath = new DeadReckonPath();
        lowerPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 60, VivaldiCalibration.LIFT_DOWN);


        // depot path
        exitDepotPath = new DeadReckonPath();
        exitDepotPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.2);
        knockPath = new DeadReckonPath();
        marker.setPosition(VivaldiCalibration.MARKER_STOWED);
        dropoff = new MarkerDropoff2();
        landingEncoderTask = new RunToEncoderValueTask(this, lift, 26000, VivaldiCalibration.LIFT_DOWN);

        // initializing gamepad variables
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        // initializing telemetry items
        allianceItem = telemetry.addData("Alliance: ", "NOT SELECTED");
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        dropMarkerItem = telemetry.addData("Drop Marker", "NOT SELECTED");
        landedItem = telemetry.addData("Robot Landed", "Still hanging around");

        // initializing mineral detection
        initializeMineralDetection();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                if (updatedMinerals != null) {
                    numberOfMineralsItem.setValue(updatedMinerals.size());
                    int goldMineralX = -1;
                    int silverMineral1X = -1;
                    int silverMineral2X = -1;
                    for (Recognition recognition : updatedMinerals) {
                        if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                            goldMineralX = (int) recognition.getLeft();
                        } else if (silverMineral1X == -1) {
                            silverMineral1X = (int) recognition.getLeft();
                        } else {
                            silverMineral2X = (int) recognition.getLeft();
                        }
                    }
                    if (goldMineralX != -1 && silverMineral1X != -1 && silverMineral2X != -1) {
                        if (goldMineralX < silverMineral1X && goldMineralX < silverMineral2X) {
                            goldMineralPositionItem.setValue("LEFT");
                            goldMineralPosition = MarkerDropoff2.GoldMineralPosition.LEFT;
                        } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                            goldMineralPositionItem.setValue("RIGHT");
                            goldMineralPosition = MarkerDropoff2.GoldMineralPosition.RIGHT;
                        } else {
                            goldMineralPositionItem.setValue("CENTER");
                            goldMineralPosition = MarkerDropoff2.GoldMineralPosition.CENTER;
                        }
                    }
                }

            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }

    @Override
    public void start() {
        IMULevelSensorCriteria imuSensor = new IMULevelSensorCriteria(imu, 5.0);
        lowerTask = new DeadReckonTask(this, lowerPath, caribinerDrivetrain, imuSensor) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent)e;
                if (event.kind == EventKind.SENSOR_SATISFIED) {
                    landedItem.setValue("Lander down");
                    lowerTask.stop();
                }
            }
        };
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    allianceColor = AllianceColor.BLUE;
                    allianceItem.setValue("BLUE");
                    break;
                case BUTTON_B_DOWN:
                    allianceColor = AllianceColor.RED;
                    allianceItem.setValue("RED");
                    break;
                case RIGHT_BUMPER_DOWN:
                    dropMarker = MarkerDropoff2.DropMarker.TRUE;
                    dropMarkerItem.setValue("TRUE");
                    break;
                case LEFT_BUMPER_DOWN:
                    dropMarker = MarkerDropoff2.DropMarker.FALSE;
                    dropMarkerItem.setValue("FALSE");
                    break;
            }
        } else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            handleLandingDoneEvent(event);
        }
    }

    protected void handleLandingDoneEvent(RunToEncoderValueTask.RunToEncoderValueEvent e) {
        if (e.kind == RunToEncoderValueTask.EventKind.DONE) {
            addTask(new RunToEncoderValueTask(this, lift, 800, VivaldiCalibration.LIFT_UP) {
                public void handleEvent(RobotEvent e) {
                    RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
                    if (event.kind == EventKind.DONE) {
                        knockPath = dropoff.getPath(dropMarker, goldMineralPosition);
                        initialMove(knockPath);
                    }
                }
            });
        }
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                    markerDrop();
                }
            }
        });
    }

    protected void markerDrop() {
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                }
            }
        });
    }
}
