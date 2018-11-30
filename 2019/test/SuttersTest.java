package test;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import org.firstinspires.ftc.robotcore.external.ClassFactory;
import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.Camera;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.MineralDetectionTask;
import team25core .Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "Sutter's Test")
public class SuttersTest extends Robot {
    // declaring telemetry items
    private Telemetry.Item numberOfMinerals;
    private Telemetry.Item goldMineralPosition;

    // declaring drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    // declaring DeadReckonPaths
    private DeadReckonPath knockLeftPath;
    private DeadReckonPath knockCenterPath;
    private DeadReckonPath knockRightPath;
    private DeadReckonPath knockPath;

    // declaring mineral detection variables
    private VuforiaLocalizer vuforia;
    private static final String VUFORIA_KEY = "AdLmvUj/////AAAAGe/kAsI/H0WukR1Af5Og5w2Ey6b+wOXQ0h30RtwQyvYckcYCH8CBcrs0EGIqrGt0wbi7/icc/5DO3kqFkMdUh41bqjMCXWLU4d3Bz35AwPn89qCf/zp+ggEwgIUry20vwpU4uACQEqOJox8PHwzBmax9PquM/Jiq+/6wTx+8Bnd3Io4ymylg2uTVOsumVcphYhjkSyzaT+sUYtXGEdVEMWdyny8WuK4RE1SsaVLOvYap++/pA9b/7LLOFqW3yAwkaDMrPeqkCIN7RnDwH0ZxTbHsRRC/xKl43igL1T02tg0eUmeeyHdUxjP8T9BQlCdDmZvA5wGg6AAqe2ORWauhS49UvjW5xLGxglnsXXm0N4ce";

    @Override
    public void handleEvent(RobotEvent e) {
        // keep scrolling...nothing to see here
    }

    @Override
    public void init() {
        // initializing telemetry
        numberOfMinerals = telemetry.addData("Number of Minerals: ", "NOT SELECTED");
        goldMineralPosition = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");


        // initializing mineral detection
        initializeMineralDetection();
        initializeVuforia();

        // initializing drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        // initializing paths
        knockLeftPath = new DeadReckonPath();
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.TURN, 40, -0.3);
        knockLeftPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);

        knockCenterPath = new DeadReckonPath();
        knockCenterPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.3);

        knockRightPath = new DeadReckonPath();
        knockRightPath.addSegment(DeadReckonPath.SegmentType.TURN, 40, 0.3);
        knockRightPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.5);
    }

    protected void initializeMineralDetection() {
        MineralDetectionTask mdTask = new MineralDetectionTask(this) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent)e;
                List<Recognition> updatedMinerals = event.minerals;
                if (updatedMinerals != null) {
                    numberOfMinerals.setValue(updatedMinerals.size());
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
                            goldMineralPosition.setValue("LEFT");
                            knockPath = knockLeftPath;
                        } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                            goldMineralPosition.setValue("RIGHT");
                            knockPath = knockRightPath;
                        } else {
                            goldMineralPosition.setValue("CENTER");
                            knockPath = knockCenterPath;
                        }
                    }
                }

            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        // add marker paths later on
                }
            }
        });
    }

    private void initializeVuforia (){
        VuforiaLocalizer.Parameters paramters = new VuforiaLocalizer.Parameters();

        paramters.vuforiaLicenseKey = VUFORIA_KEY;
        paramters.cameraName = hardwareMap.get(WebcamName.class, "mineralCamera");

        vuforia = ClassFactory.getInstance().createVuforia(paramters);
    }

    @Override
    public void start() {
        initialMove(knockPath);
    }

}