package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.ServoCalibrateTask;

/**
 * Created by Lizzie on 12/9/2017.
 */
@Autonomous(name = "Servo Calibrate Test")
public class ServoCalibrateTest extends Robot {

    private Servo jewelXServo;
    private ServoCalibrateTask servoTask;

    @Override
    public void init() {
        jewelXServo = hardwareMap.servo.get("jewelXAxis");
        servoTask = new ServoCalibrateTask(this, jewelXServo);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        this.addTask(servoTask);
    }
}
