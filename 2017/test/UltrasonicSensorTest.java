package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.UltrasonicSensor;

import team25core.GamepadTask;
import team25core.MonitorUltrasonicSensorTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Izzie on 2/20/2016.
 */
@Autonomous(name = "TEST Ultrasonic", group = "TEST")
public class UltrasonicSensorTest extends Robot {

    private UltrasonicSensor rightSound;

    private MonitorUltrasonicSensorTask monitorRight;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {

        rightSound = hardwareMap.ultrasonicSensor.get("rightSound");
        monitorRight = new MonitorUltrasonicSensorTask(this, rightSound);
    }

    @Override
    public void start() {
        addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;

                switch(event.kind) {
                    case BUTTON_Y_DOWN:
                        removeTask(monitorRight);
                        break;
                    case BUTTON_A_DOWN:
                        addTask(monitorRight);
                        break;
                }
            }
        });
    }

}
