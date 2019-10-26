package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;

@TeleOp(name = "LM0 Telop")
public class MozartLM0Teleop extends Robot {
    // teleop with the mecanum drivetrain and linear lift
    // active wheel intake

    /* GAMEPAD 2
    // linear lift up (right bumper) down (right trigger)
    // active wheel intake in (A) out (B)
     */

    /* GAMEPAD 1
    // drivetrain
    // slow mode!
     */
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private MechanumGearedDrivetrain drivetrain;
    private TeleopDriveTask driveTask;
    private DcMotor leftIntake;
    private DcMotor rightIntake;
    private DcMotor lift;
    private Servo claw;

    public void handleEvent (RobotEvent e) {

    }

    public void init()
    {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft"); //drivetrain
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");
        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1);
        drivetrain = new MechanumGearedDrivetrain(60, frontLeft,frontRight, backLeft, backRight);
        driveTask = new TeleopDriveTask(this, scheme, frontLeft, frontRight, backLeft, backRight);
        leftIntake = hardwareMap.get(DcMotor.class, "leftIntake");
        rightIntake = hardwareMap.get(DcMotor.class, "rightIntake");
        lift = hardwareMap.dcMotor.get("lift");
        claw = hardwareMap.servo.get("claw");
        // GamepadTask gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        // addTask(gamepad);
    }

    @Override
    public void start()
    {
        this.addTask(driveTask);

        // GAMEPAD 2
        DeadmanMotorTask liftUp = new DeadmanMotorTask(this, lift, HisaishiCalibration.LIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        addTask(liftUp);
        DeadmanMotorTask liftDown = new DeadmanMotorTask(this, lift, HisaishiCalibration.LIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        addTask(liftDown);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.LEFT_BUMPER_DOWN) {
                    leftIntake.setPower(HisaishiCalibration.INTAKE_LEFT_COLLECT);
                    rightIntake.setPower(HisaishiCalibration.INTAKE_RIGHT_COLLECT);
                } else if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                    leftIntake.setPower(HisaishiCalibration.INTAKE_LEFT_DISPENSE);
                    rightIntake.setPower(HisaishiCalibration.INTAKE_RIGHT_DISPENSE);
                } else if (event.kind == EventKind.LEFT_TRIGGER_UP || event.kind == EventKind.LEFT_BUMPER_UP) {
                    leftIntake.setPower(0.0);
                    rightIntake.setPower(0.0);
                }
            }
        });
    }
}
