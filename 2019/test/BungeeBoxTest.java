package test;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import opmodes.Utilities.VivaldiCalibration;
import team25core.GamepadTask;
import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;

/**
 * Created by Lizzie on 11/24/2018.
 */
@TeleOp(name = "Bungee Box Test")
@Disabled
public class BungeeBoxTest extends Robot {
    private DcMotor bungeeBox;

    @Override
    public void init() {
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
    }

    protected void lowerBBox() {
        RunToEncoderValueTask lowerBoxTask = new RunToEncoderValueTask(this, bungeeBox, VivaldiCalibration.BUNGEE_ENCODER_COUNT, VivaldiCalibration.BUNGEE_BOX_DEPLOY) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    bungeeBox.setPower(0.0);
                }
            }
        };
        addTask(lowerBoxTask);
    }

    @Override
    public void start() {
       this.addTask(new MonitorMotorTask(this, bungeeBox));
       this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
           public void handleEvent(RobotEvent e) {
               GamepadEvent event = (GamepadEvent) e;
               if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                   lowerBBox();
               } else if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                   bungeeBox.setPower(0.0);
               }
           }
       });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
