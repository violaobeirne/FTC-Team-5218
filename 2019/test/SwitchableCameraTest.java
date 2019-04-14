package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.ClassFactory;
import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.SwitchableCamera;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;
import org.firstinspires.ftc.robotcore.internal.camera.delegating.SwitchableCameraName;

import java.util.List;

import opmodes.Utilities.VivaldiCalibration;
import team25core.GamepadTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.VuforiaConstants;

/**
 * Created by Lizzie on 3/30/2019.
 */
@Autonomous(name = "Switchable Camera Test")
public class SwitchableCameraTest extends Robot{
    private SwitchableCamera switchableCamera;
    private VuforiaLocalizer vuforia;
    private WebcamName webCam1;
    private WebcamName webCam2;

    private MineralDetectionTask mdTask;
    int goldMineralX;

    private final static String TAG = "SwitchableCamera";

    @Override
    public void init() {
        initializeMineralDetection();
    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    RobotLog.ii(TAG, "Toggle camera");
                    mdTask.toggleCamera();
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    protected void initializeMineralDetection() {
        mdTask = new MineralDetectionTask(this, "mineralCamera2", "mineralCamera1") {
            public void handleEvent (RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                Recognition goldMineral;
                List<Recognition> singletonMineralList = event.minerals;
                goldMineral= singletonMineralList.get(0);
                goldMineralX = (int) goldMineral.getLeft();
                int imageCenter = goldMineral.getImageWidth() / 2;
                int mineralCenter = ((int)goldMineral.getWidth() / 2) + goldMineralX;
                int offset = java.lang.Math.abs(imageCenter - mineralCenter);
                RobotLog.ii(TAG, "Location data %d/%d/%d", imageCenter, mineralCenter, offset);
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.LARGEST_GOLD);
        addTask(mdTask);
    }
}
