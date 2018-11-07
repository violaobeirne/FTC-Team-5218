package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 10/20/2018.
 */
@TeleOp(name = "League Meet 0 Teleop")
public class LeagueMeet0Teleop extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor bbExtension;
    private FourWheelDirectDrivetrain drivetrain;
    private TankDriveTask driveTask;

    private Servo bungeeBox;

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        bbExtension = hardwareMap.dcMotor.get("bbExtension");
        bungeeBox = hardwareMap.servo.get("bungeeBox");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        driveTask = new TankDriveTask(this, drivetrain);
    }
    @Override
    public void start() {
        driveTask = new TankDriveTask(this, drivetrain);
        addTask(driveTask);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_STOWED);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_MIDDLE);
                } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                    bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_DEPLOYED);
                } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                    bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_LOW_DEPLOYED);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    bbExtension.setPower(1.0);
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    bbExtension.setPower(-1.0);
                } else if (event.kind == EventKind.RIGHT_BUMPER_UP || event.kind == EventKind.RIGHT_TRIGGER_UP) {
                    bbExtension.setPower(0.0);
                    bbExtension.setPower(0.0);
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
