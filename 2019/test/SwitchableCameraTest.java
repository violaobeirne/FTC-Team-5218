package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.ClassFactory;
import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.hardware.camera.SwitchableCamera;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.navigation.VuforiaLocalizer;
import org.firstinspires.ftc.robotcore.internal.camera.delegating.SwitchableCameraName;

import opmodes.Utilities.VivaldiCalibration;
import team25core.GamepadTask;
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

    @Override
    public void init() {
        webCam1 = hardwareMap.get(WebcamName.class, "mineralCamera1");
        webCam2 = hardwareMap.get(WebcamName.class, "mineralCamera2");

        SwitchableCameraName switchableCameraName = ClassFactory.getInstance().getCameraManager().nameForSwitchableCamera(webCam1, webCam2);
        int cameraMonitorViewId = hardwareMap.appContext.getResources().getIdentifier("cameraMonitorViewId", "id", hardwareMap.appContext.getPackageName());
        VuforiaLocalizer.Parameters parameters = new VuforiaLocalizer.Parameters(cameraMonitorViewId);
        parameters.vuforiaLicenseKey = VuforiaConstants.VUFORIA_KEY;

        parameters.cameraName = switchableCameraName;
        this.vuforia = ClassFactory.getInstance().createVuforia(parameters);
        this.switchableCamera = (SwitchableCamera)vuforia.getCamera();

    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                   toggleCamera();
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {

                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    public void toggleCamera() {
        if (switchableCamera != null) {
            if (switchableCamera.getActiveCamera().equals(webCam1)) {
                switchableCamera.setActiveCamera(webCam2);
                RobotLog.i("4042: Switching to camera 2.");
            } else {
                switchableCamera.setActiveCamera(webCam1);
                RobotLog.i("4042: Switching to camera 1.");
            }
        }
    }
}
