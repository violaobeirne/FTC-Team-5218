package opmodes.LM3;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import opmodes.Paths.DoubleSamplingDropoff;
import opmodes.Paths.ExitDepotDropoff;
import opmodes.Paths.MineralMarkerDropoff;
import opmodes.Utilities.MineralUtils;
import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMULevelSensorCriteria;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "League Meet 3 Mineral Autonomous")
@Disabled
public class LeagueMeet3BeethovenAutonomous extends Robot {

    private static final String TAG = "Lizzie Auto";

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
    private FourWheelDirectDrivetrain drivetrain;
    private BNO055IMU imu;
    private Servo marker;

    private DeadReckonPath knockPath;
    private MineralMarkerDropoff dropoff;

    private DeadReckonPath exitDepotPath;
    private ExitDepotDropoff depotDropoff;

    private DeadReckonPath doubleSamplingPath;
    private DoubleSamplingDropoff doubleSamplingDropoff;
    private boolean dropMarkerBoolean = false;
    private boolean doubleSamplingBoolean = false;
    // private DcMotor bungeeBox;

    // declaring gamepad variables
    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected MineralUtils.MineralPosition goldMineralPosition;
    protected MineralUtils.DropMarker dropMarker;

    // declaring telemetry item
    private Telemetry.Item allianceItem;
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item dropMarkerItem;
    private Telemetry.Item doubleSampling;
    private IMULevelSensorCriteria imuSensor;

    @Override
    public void init() {
        // initializing motors, servos, and drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        marker = hardwareMap.servo.get("marker");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        // bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        // bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        // bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);

        imu = hardwareMap.get(BNO055IMU.class, "imu");

        // depot path
        knockPath = new DeadReckonPath();
        dropoff = new MineralMarkerDropoff();
        exitDepotPath = new DeadReckonPath();
        depotDropoff = new ExitDepotDropoff();
        doubleSamplingPath = new DeadReckonPath();
        doubleSamplingDropoff = new DoubleSamplingDropoff();

        // initializing gamepad variables
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        // initializing telemetry items
        allianceItem = telemetry.addData("Alliance: ", "NOT SELECTED");
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        dropMarkerItem = telemetry.addData("Drop Marker", "NOT SELECTED");
        doubleSampling = telemetry.addData("Double Sampling: ", "NOT SELECTED");

        imuSensor = new IMULevelSensorCriteria(imu, 2.86);

        // initializing mineral detection
        marker.setPosition(VivaldiCalibration.MARKER_STOWED);
        initializeMineralDetection();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                numberOfMineralsItem.setValue(updatedMinerals.size());
                MineralUtils.MineralPosition goldPos = MineralUtils.determineGoldPosition(updatedMinerals);
                MineralUtils.sendPositionTelemetry(goldPos, goldMineralPositionItem);

                switch (goldPos) {
                    case LEFT:
                        goldMineralPosition = MineralUtils.MineralPosition.LEFT;
                        break;
                    case RIGHT:
                        goldMineralPosition = MineralUtils.MineralPosition.RIGHT;
                        break;
                    case CENTER:
                        goldMineralPosition = MineralUtils.MineralPosition.CENTER;
                        break;
                    case UNKNOWN:
                        goldMineralPosition = MineralUtils.MineralPosition.CENTER;
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
        knockPath = dropoff.getPath(dropMarker, goldMineralPosition);
        exitDepotPath = depotDropoff.getPath(dropMarker, goldMineralPosition);
        doubleSamplingPath = doubleSamplingDropoff.getPath(dropMarker, goldMineralPosition);

        initialMove(knockPath);
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
                    dropMarker = MineralUtils.DropMarker.TRUE;
                    dropMarkerBoolean = true;
                    dropMarkerItem.setValue("TRUE");
                    break;
                case LEFT_BUMPER_DOWN:
                    dropMarker = MineralUtils.DropMarker.FALSE;
                    dropMarkerBoolean = false;
                    dropMarkerItem.setValue("FALSE");
                    break;
                case RIGHT_TRIGGER_DOWN:
                    doubleSampling.setValue("TRUE");
                    doubleSamplingBoolean = true;
                    break;
                case LEFT_TRIGGER_DOWN:
                    doubleSampling.setValue("FALSE");
                    doubleSamplingBoolean = false;
                    break;
            }
        }
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if(dropMarkerBoolean == true) {
                            marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                            markerDrop();
                        }
                }
            }
        });
    }

    protected void markerDrop() {
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                if(dropMarkerBoolean == true) {
                    exitDepot(exitDepotPath);
                }
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if(doubleSamplingBoolean == true) {
                            secondSampling(doubleSamplingPath);
                        } else if (doubleSamplingBoolean == false) {

                        }
                }
            }
        });
    }

    protected void secondSampling (final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:

                }
            }
        });
    }
}
