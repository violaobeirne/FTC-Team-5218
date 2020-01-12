package test;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 1/11/2020.
 */

@TeleOp(name = "Ticks Per Inch")
public class TicksPerInch extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {
        backRight = hardwareMap.get(DcMotor.class, "backRight");

        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    @Override
    public void loop() {
        telemetry.addData("Encoder", backRight.getCurrentPosition());
    }

    @Override
    public void start() {
    }

}
