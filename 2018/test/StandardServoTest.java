package test;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.ServoCalibrateTask;

/**
 * Created by Lizzie on 9/23/2017.
 */

@TeleOp(name="TEST Servo")
@Disabled
public class StandardServoTest extends Robot{

    private Servo servo;

    @Override
    public void init() {
        // servo
        servo = hardwareMap.servo.get("jewelXAxis");
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        ServoCalibrateTask servoTask = new ServoCalibrateTask(this, servo, 128);
        addTask(servoTask);
    }
}
