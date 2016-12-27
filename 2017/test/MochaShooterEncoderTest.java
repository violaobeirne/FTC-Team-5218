package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunAtRpmTask;
import team25core.RunToEncoderValueTask;

/**
 * Created by Lizzie on 12/3/2016.
 */
@Autonomous(name = "TEST Shooter", group = "AutoTest")
public class MochaShooterEncoderTest extends Robot{

    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private int positionFL;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {
        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");
        shooterLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    @Override
    public void start() {
        shooterLeft.setPower(0.1);
        shooterRight.setPower(0.1);
        // addTask(new MonitorMotorTask(this, shooterLeft, MonitorMotorTask.MotorKind.ANDYMARK_3_7, MonitorMotorTask.DISPLAY_RPM));
        addTask(new RunAtRpmTask(this, shooterRight, MonitorMotorTask.MotorKind.ANDYMARK_3_7, 900));
    }

}
