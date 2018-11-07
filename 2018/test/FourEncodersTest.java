package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.LightSensor;

import team25core.PersistentTelemetryTask;

/**
 * Created by Lizzie on 12/1/2017.
 */
@Autonomous(name = "Drivetrain Encoder Test")
public class FourEncodersTest extends OpMode{

    private DcMotor frontLeft;
    private DcMotor backLeft;
    private DcMotor frontRight;
    private DcMotor backRight;

    private int positionFL;
    private int positionFR;
    private int positionRL;
    private int positionRR;


    @Override
    public void init()
    {
        frontLeft = hardwareMap.dcMotor.get("frontL");
        backLeft = hardwareMap.dcMotor.get("backL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backRight = hardwareMap.dcMotor.get("backR");

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        // frontLeft.setDirection(DcMotorSimple.Direction.REVERSE);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        frontRight.setDirection(DcMotorSimple.Direction.REVERSE);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        backLeft.setDirection(DcMotorSimple.Direction.REVERSE);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        // backRight.setDirection(DcMotorSimple.Direction.REVERSE );


    }

    @Override
    public void loop()
    {
        frontLeft.setPower(0.1);
        frontRight.setPower(0.1);
        backLeft.setPower(0.1);
        backRight.setPower(0.1);

        positionFL = Math.abs(frontLeft.getCurrentPosition());
        telemetry.addData("Front Left Position: ", positionFL);
        positionFR = Math.abs(frontRight.getCurrentPosition());
        telemetry.addData("Front Right Position: ", positionFR);
        positionRL = Math.abs(backLeft.getCurrentPosition());
        telemetry.addData("back Left Position: ", positionRL);
        positionRR = Math.abs(backRight.getCurrentPosition());
        telemetry.addData("back Right Position: ", positionRR);
    }
}