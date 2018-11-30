package test;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import org.firstinspires.ftc.robotcore.external.ClassFactory;
import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.Camera;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;
import org.firstinspires.ftc.robotcore.external.tfod.TFObjectDetector;

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
    private static final String TFOD_MODEL_ASSET = "RoverRuckus.tflite";
    private static final String LABEL_GOLD_MINERAL = "Gold Mineral";
    private static final String LABEL_SILVER_MINERAL = "Silver Mineral";
    private static final String VUFORIA_KEY = " ASqh/rz/////AAABmY9M05H8VUa9iKDQrhKP2MmITNlky9LsYv0PfLC4L6MmVTGzL/M3vTfg5WZclp7+8TpRy0gR4q60axtlPTRgEEhN5hRcoLzWcv22WMQc/4jnZ/JU493ZG7QEEbURrKmlPO1PcbeaIq6uRpGz07jLyhnNs+lqwP8nQGfEgxZOOuNlBOteSr/A9jacpx+bHbwyfDuqBZPRGb9oqRI8jYS8pVL4nX0AA4RAcfimis5qWbW6FGKlU9L2W8AwaBd75gh4dQkO3zFItsOhgNVNcFF0COKiJBf4MOFAuWUPPrKrN31lhqGvl0rCK9SX7MrR3OwVSvaA06Zt5wnJ5y7/lYOYF8FTHfO22uIkZz73lOZOwors";

    private VuforiaLocalizer vuforia;

    private TFObjectDetector tfod;

    @Override
    public void handleEvent(RobotEvent e) {
        // keep scrolling...nothing to see here
    }

    @Override
    public void init() {
        // initializing telemetry
        numberOfMinerals = telemetry.addData("Number of Minerals: ", "NOT SELECTED");
        goldMineralPosition = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");

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

        // gold mineral detection

        initializeVuforia();
        if ((ClassFactory.getInstance().canCreateTFObjectDetector()) && (tfod != null)) {
            initializeTfod();
            tfod.activate();

            List<Recognition> updatedMinerals = tfod.getUpdatedRecognitions();
            if (updatedMinerals != null) {
                numberOfMinerals.setValue(updatedMinerals.size());
                if (updatedMinerals.size() == 3) {
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
        }
        if (tfod != null) {
            tfod.shutdown();
        }

    }

    /*
    protected void initializeMineralDetection() {
        MineralDetectionTask mdTask = new MineralDetectionTask(this) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent)e;
                List<Recognition> updatedMinerals = event.minerals;
                if (updatedMinerals != null) {
                    numberOfMinerals.setValue(updatedMinerals.size());
                    if (updatedMinerals.size() == 3) {
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

            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }
    */

    private void initializeVuforia (){
        VuforiaLocalizer.Parameters parameters = new VuforiaLocalizer.Parameters();
        parameters.vuforiaLicenseKey = VUFORIA_KEY;
        parameters.cameraName = hardwareMap.get(WebcamName.class, "mineralCamera");
        vuforia = ClassFactory.getInstance().createVuforia(parameters);
    }

    private void initializeTfod() {
        int tfodMonitorViewId = hardwareMap.appContext.getResources().getIdentifier(
                "tfodMonitorViewId", "id", hardwareMap.appContext.getPackageName());
        TFObjectDetector.Parameters tfodParameters = new TFObjectDetector.Parameters(tfodMonitorViewId);
        tfod = ClassFactory.getInstance().createTFObjectDetector(tfodParameters, vuforia);
        tfod.loadModelFromAsset(TFOD_MODEL_ASSET, LABEL_GOLD_MINERAL, LABEL_SILVER_MINERAL);
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
    @Override
    public void start() {
        initialMove(knockPath);
    }

}