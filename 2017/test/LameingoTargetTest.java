package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.Robot_MecanumDrive;
import team25core.Robot_Navigation;
import team25core.Robot_TwoWheelDrive;

/*
 * FTC Team 25: cmacfarl, January 31, 2017
 */

@Autonomous(name = "TEST Target", group = "AutoTest")
public class LameingoTargetTest extends Robot {

    Robot_Navigation nav;
    Robot_TwoWheelDrive drive;
    DcMotor leftMotor;
    DcMotor rightMotor;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init()
    {
        drive = new Robot_TwoWheelDrive();
        drive.initDrive(this);
        nav = new Robot_Navigation();
        nav.initVuforia(this, drive);
    }

    @Override
    public void start()
    {
    }

    @Override
    public void loop()
    {
        super.loop();

        telemetry.addData("Target visible", nav.targetsAreVisible());

        telemetry.update();
    }
}
