package test;/*
 * Created by izzielau on 2/4/2017.
 */

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.ServoCalibrateTask;

@TeleOp (name = "TEST Servo", group = "TEST")
public class MochaServoCalibration extends Robot {

    public Servo servo;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init()
    {
        servo = hardwareMap.servo.get("beacon");
    }

    @Override
    public void start()
    {
        addTask(new ServoCalibrateTask(this, servo));
    }


}
