package test;

import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 9/23/2017.
 */

public class StandardServoTest extends Robot{

    private Servo servo;

    @Override
    public void init() {
        // servo
        servo = hardwareMap.servo.get("servo");
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;

                if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    servo.setPosition(1.0);
                } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                    servo.setPosition(0);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    servo.setPosition(-1.0);
                }
            }
        });
    }
}
