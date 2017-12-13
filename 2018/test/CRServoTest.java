package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.CRServo;

import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/9/2017.
 */
@Autonomous(name = "CRServo Test")
public class CRServoTest extends Robot {
    private CRServo glyphSlider;

    @Override
    public void init() {
        glyphSlider = hardwareMap.crservo.get("glyphSlider");
    }

    @Override
    public void handleEvent(RobotEvent e) {
        //...keep scrolling
    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_X_DOWN) {
                    glyphSlider.setPower(0);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    glyphSlider.setPower(255);
                } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                    glyphSlider.setPower(128);
                }
            }
        });
    }
}

