package opmodes.LM2;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;

@TeleOp(name = "5218 LM2 Teleop")
@Disabled
public class RachmaninoffLM2Teleop extends Robot {
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
    private DcMotor vLift;
    private Servo claw;
    private Servo leftArm;
    private Servo rightArm;
    private Servo arm;
    private CRServo hLift;
    private DcMotor leftIntake;
    private DcMotor rightIntake;

    public void handleEvent(RobotEvent e) {

    }

    public void init() {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");
        leftArm = hardwareMap.servo.get("leftArm");
        rightArm = hardwareMap.servo.get("rightArm");
        arm = hardwareMap.servo.get("arm");
        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.setCanonicalMotorDirection();
        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1, TankMechanumControlScheme.MotorDirection.NONCANONICAL);
        driveTask = new TeleopDriveTask(this, MiyazakiCalibration.SPEED_LIMIT, scheme, frontLeft, frontRight, backLeft, backRight);
        vLift = hardwareMap.dcMotor.get("vLift");
        hLift = hardwareMap.crservo.get("hLift");
        leftIntake = hardwareMap.dcMotor.get("leftIntake");
        rightIntake = hardwareMap.dcMotor.get("rightIntake");
        claw = hardwareMap.servo.get("claw");
    }

    @Override
    public void start() {
        this.addTask(driveTask);

        // GAMEPAD 2
        DeadmanMotorTask liftUp = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        addTask(liftUp);
        DeadmanMotorTask liftDown = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        addTask(liftDown);
        DeadmanMotorTask leftIntakeIn = new DeadmanMotorTask(this, leftIntake, MiyazakiCalibration.INTAKE_LEFT_COLLECT, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(leftIntakeIn);
        DeadmanMotorTask rightIntakeIn = new DeadmanMotorTask(this, rightIntake, MiyazakiCalibration.INTAKE_RIGHT_COLLECT, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(rightIntakeIn);
        DeadmanMotorTask leftIntakeOut = new DeadmanMotorTask(this, leftIntake, MiyazakiCalibration.INTAKE_LEFT_DISPENSE, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_B);
        addTask(leftIntakeOut);
        DeadmanMotorTask rightIntakeOut = new DeadmanMotorTask(this, rightIntake, MiyazakiCalibration.INTAKE_RIGHT_DISPENSE, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_B);
        addTask(rightIntakeOut);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_X_DOWN:
                        claw.setPosition(MiyazakiCalibration.NEW_CLAW_OPEN);
                        break;
                    case BUTTON_Y_DOWN:
                        claw.setPosition(MiyazakiCalibration.NEW_CLAW_CLOSE);
                        break;
                    case LEFT_BUMPER_DOWN:
                        hLift.setPower(MiyazakiCalibration.HLIFT_OUT);
                        break;
                    case LEFT_TRIGGER_DOWN:
                        hLift.setPower(MiyazakiCalibration.HLIFT_IN);
                        break;
                    case LEFT_BUMPER_UP: case LEFT_TRIGGER_UP:
                        hLift.setPower(MiyazakiCalibration.HLIFT_STOP);
                        break;
                }

            }
        });

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_X_DOWN:
                        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
                        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
                        break;
                    case BUTTON_Y_DOWN:
                        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_DOWN);
                        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_DOWN);
                        break;
                    case RIGHT_TRIGGER_DOWN:
                        // arm.setPosition(MiyazakiCalibration.ARM_DOWN);
                        break;
                    case RIGHT_BUMPER_DOWN:
                        // arm.setPosition(MiyazakiCalibration.ARM_STOW);
                        break;
                    case BUTTON_A_DOWN:
                        driveTask.slowDown(false);
                        break;
                    case BUTTON_B_DOWN:
                        driveTask.slowDown(true);
                        break;
                }
            }
        });
    }
}
