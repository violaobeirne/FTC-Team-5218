package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 11/24/2018.
 */
@TeleOp(name = "Scissor Lift Test")
public class ScissorLiftTest extends Robot {
    private DcMotor lift;

    @Override
    public void init() {
        lift = hardwareMap.dcMotor.get("lift");
    }

    @Override
    public void start() {
       this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
           public void handleEvent(RobotEvent e) {
               GamepadEvent event = (GamepadEvent) e;
               if (event.kind == EventKind.BUTTON_A_DOWN) {
                   lift.setPower(1.0);
               } else if (event.kind == EventKind.BUTTON_A_DOWN.BUTTON_B_DOWN) {
                   lift.setPower(-1.0);
               } else if (event.kind == EventKind.BUTTON_A_UP || event.kind == EventKind.BUTTON_B_UP) {
                   lift.setPower(0.0);
               }
           }
       });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
