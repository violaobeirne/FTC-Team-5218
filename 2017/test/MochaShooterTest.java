package test;

/*
 * Created by izzielau on 1/31/2017.
 */

import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Gamepad;

import team25core.PersistentTelemetryTask;
import team25core.RobotEvent;

@TeleOp(name = "TEST Shooter Speeds", group = "TEST")
public class MochaShooterTest extends OpMode {

    Gamepad gamepad;
    PersistentTelemetryTask telemetry;

    protected double shooterSpeed = 0;

    DcMotor frontRight;
    DcMotor frontLeft;
    DcMotor backRight;
    DcMotor backLeft;
    DcMotor shooterLeft;
    DcMotor shooterRight;

    @Override
    public void init()
    {
        // Drivetrain.
        frontRight = hardwareMap.dcMotor.get("motorFR");
        frontLeft = hardwareMap.dcMotor.get("motorFL");
        backRight = hardwareMap.dcMotor.get("motorBR");
        backLeft = hardwareMap.dcMotor.get("motorBL");

        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        // Flywheels.
        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        shooterLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        shooterRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        gamepad = gamepad1;
    }

    @Override
    public void loop()
    {
        if (gamepad.left_bumper) {
            shooterSpeed += 0.05;
        } else if (gamepad.right_bumper) {
            shooterSpeed -= 0.05;
        }

        shooterLeft.setPower(shooterSpeed);
        shooterRight.setPower(-shooterSpeed);
    }
}
