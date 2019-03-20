package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/10/2018.
 */
@Autonomous(name = "Motor Test")
@Disabled
public class MotorTest extends Robot {

    DcMotor motor1;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {
        motor1 = hardwareMap.get(DcMotor.class, "frontRight");
        motor1.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        motor1.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    @Override
    public void start() {
        motor1.setPower(1.0);
    }
}
