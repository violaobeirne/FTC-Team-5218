package test;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import opmodes.calibration.HisaishiCalibration;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 10/2/2019.
 */
@TeleOp(name = "Lift Test")
@Disabled
public class LiftTest extends Robot {
    private DcMotor lift;

    @Override
    public void init() {
        lift = hardwareMap.dcMotor.get("lift");
        GamepadTask gamepad= new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);
    }

    @Override
    public void handleEvent(RobotEvent e) {

        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    lift.setPower(HisaishiCalibration.LIFT_UP);
                    break;
                case BUTTON_Y_DOWN:
                    lift.setPower(HisaishiCalibration.LIFT_DOWN);
                    break;

            }
        }
    }

    @Override
    public void start() {

    }
}
