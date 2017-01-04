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
@TeleOp(name = "TEST Ultrasonic", group = "5218")
public class UltrasonicSensorTest extends Robot {
    private UltrasonicSensor leftSound;
    private UltrasonicSensor rightSound;

    private MonitorUltrasonicSensorTask monitorLeft;
    private MonitorUltrasonicSensorTask monitorRight;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {

        leftSound = hardwareMap.ultrasonicSensor.get("leftSonic");
        rightSound = hardwareMap.ultrasonicSensor.get("rightSonic");

        monitorLeft = new MonitorUltrasonicSensorTask(this, leftSound);

        monitorRight = new MonitorUltrasonicSensorTask(this, rightSound);
    }

    @Override
    public void start() {
        addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;

                switch(event.kind) {
                    case BUTTON_Y_DOWN:
                        addTask(monitorLeft);
                        removeTask(monitorRight);
                        break;
                    case BUTTON_A_DOWN:
                        addTask(monitorRight);
                        removeTask(monitorLeft);
                        break;
                }
            }
        });
    }

}
