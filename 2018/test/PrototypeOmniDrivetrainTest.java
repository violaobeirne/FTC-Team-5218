package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 9/23/2017.
 */

// @TeleOp(name = "OmniDrivetrainTest")
public class PrototypeOmniDrivetrainTest extends Robot {

    public DcMotor frontLeft;
    public DcMotor frontRight;
    public DcMotor backLeft;
    public DcMotor backRight;

    public MechanumGearedDrivetrain drivetrain;
    private static final int TICKS_PER_INCH = 79;

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void init()
    {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");

        drivetrain = new MechanumGearedDrivetrain(TICKS_PER_INCH, frontRight, backRight, frontLeft, backLeft);
    }

    @Override
    public void start()
    {
        this.addTask(new TankDriveTask(this, drivetrain));
    }
}
