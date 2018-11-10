package opmodes;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.FourWheelDirectDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 11/5/2018.
 */
public class LeagueMeet1HaydnAutonomous extends Robot {

    // Declare private motors and servos.
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private DcMotor scissorLift;
    private DcMotor bbExtension;
    private Servo bungeeBox;
    private Servo cameraArm;
    private Servo marker;

    private FourWheelDirectDrivetrain drivetrain;

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        scissorLift = hardwareMap.dcMotor.get("scissorLift");
        bbExtension = hardwareMap.dcMotor.get("bbExtension");
        bungeeBox = hardwareMap.servo.get("bungeeBox");
        cameraArm = hardwareMap.servo.get("cameraArm");
        marker = hardwareMap.servo.get("marker");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_STOWED);

    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {

    }
}
