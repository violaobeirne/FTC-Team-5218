package summer.Pink;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.HisaishiCalibration;
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
@TeleOp (name = "Pink Panther Teleop")
public class PinkTeleop extends Robot {

    private DcMotor left;
    private DcMotor right;

    private TwoWheelDirectDrivetrain fwd;
    private TwoWheelDriveTask driveTask;

    // add or remove servos/motors if you have any, make sure the servo name is appropriate so you can tell which one it is
    private Servo leftArm;
    private Servo rightArm;
    private CRServo linearLift;

    private double leftOpen = 0.0/256.0;
    private double leftClose = 120.0/256.0;
    private double rightOpen = 256.0/256.0;
    private double rightClose = 150.0/256.0;


    @Override
    public void init() {
        left = hardwareMap.dcMotor.get("Right");
        right = hardwareMap.dcMotor.get("Left");
        left.setDirection(DcMotorSimple.Direction.REVERSE);
        right.setDirection(DcMotorSimple.Direction.REVERSE);

        leftArm = hardwareMap.servo.get("leftServo");
        rightArm = hardwareMap.servo.get("rightServo");
        linearLift = hardwareMap.crservo.get("linearServo");

        fwd = new TwoWheelDirectDrivetrain(right, left);

        fwd.resetEncoders();
        fwd.encodersOn();

        driveTask = new TwoWheelDriveTask(this, right, left);
        driveTask.slowDown(false);
    }

    @Override
    public void start() {
        /* Driver One */
        addTask(driveTask);

        /* Driver Two */
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                RobotLog.i("Gamepad Event", event.kind);
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    leftArm.setPosition(leftOpen);
                    rightArm.setPosition(rightOpen);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    rightArm.setPosition(rightClose);
                    leftArm.setPosition(leftClose);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    linearLift.setPower(-0.3);
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    linearLift.setPower(0.3);
                } else if (event.kind == EventKind.RIGHT_BUMPER_UP || event.kind == EventKind.RIGHT_TRIGGER_UP) {
                    linearLift.setPower(0.0);
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // .. nothing to see here
    }
}
