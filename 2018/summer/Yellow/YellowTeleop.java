package summer.Yellow;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;
import team25core.TwoWheelDirectDrivetrain;
import team25core.TwoWheelDriveTask;

/**
 * Created by Lizzie on 5/16/2018.
 */
@TeleOp(name = "Bumble Bot Teleop")
public class YellowTeleop extends Robot {

    private DcMotor left;
    private DcMotor right;

    private TwoWheelDirectDrivetrain fwd;
    private TwoWheelDriveTask driveTask;

    // add or remove servos/motors if you have any, make sure the servo name is appropriate so you can tell which one it is
    private Servo arm;
    private DcMotor leftWing;
    private DcMotor rightWing;
    private double rightIntake = -0.7;
    private double leftIntake = 0.7;
    private double rightDisperse = 0.7;
    private double leftDisperse = -0.7;

    @Override
    public void init() {
        left = hardwareMap.dcMotor.get("Left");
        right = hardwareMap.dcMotor.get("Right");

        leftWing = hardwareMap.dcMotor.get("leftIn");
        rightWing = hardwareMap.dcMotor.get("rightIn");
        arm = hardwareMap.servo.get("jewel");

        fwd = new TwoWheelDirectDrivetrain(right, left);
        fwd.resetEncoders();
        fwd.encodersOn();

        driveTask = new TwoWheelDriveTask(this, right, left);
        driveTask.slowDown(false);

    }

    @Override
    public void start() {

        addTask(driveTask);
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                RobotLog.i("Gamepad Event", event.kind);
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    rightWing.setPower (rightIntake);
                    leftWing.setPower(leftIntake);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    rightWing.setPower(rightDisperse);
                    leftWing.setPower(leftDisperse);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    arm.setPosition(0.0);
                } else if (event.kind == EventKind.BUTTON_A_UP || event.kind == EventKind.BUTTON_B_UP) {
                    rightWing.setPower(0.0);
                    leftWing.setPower(0.0);
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // .. nothing to see here
    }
}
