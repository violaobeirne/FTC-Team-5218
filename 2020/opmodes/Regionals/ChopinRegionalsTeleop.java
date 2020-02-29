package opmodes.Regionals;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.hardware.TouchSensor;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.RobotEvent;
import team25core.StandardFourMotorRobot;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;
import team25core.TouchSensorCriteria;

@TeleOp(name = "5218 Regionals Teleop")
public class ChopinRegionalsTeleop extends StandardFourMotorRobot {

    private final String TAG = "Chopin";

    // enum

    public enum ArmToggle {
        STOW_ARM,
        DEPLOY_ARM
    }

    public enum drivetrainMode {
        SLOW_MODE,
        FAST_MODE,
    }
    drivetrainMode dMode = drivetrainMode.FAST_MODE;

    public enum clawMode {
        CLAW_OPEN,
        CLAW_CLOSE,
    }
    clawMode cMode = clawMode.CLAW_OPEN;

    // drivetrain
    private MechanumGearedDrivetrain drivetrain;
    private TeleopDriveTask driveTask;

    // mechanisms
    private DcMotor vLift;
    private Servo claw;
    private Servo leftArm;
    private Servo rightArm;
    private ArmToggle foundationArms;
    private Servo leftStoneArm;
    private Servo rightStoneArm;
    private Servo stoneArm;
    private ArmToggle stoneArms;
    private CRServo hLift;
    private DcMotor leftIntake;
    private DcMotor rightIntake;
    private DcMotor tapeMeasurer;
    private boolean intakeOn;

    // sensors
    private TouchSensor touchRight;
    private TouchSensor touchLeft;
    private TouchSensorCriteria touchRightCriteria;
    private TouchSensorCriteria touchLeftCriteria;
    private DeadReckonPath touchPath;
    private DeadReckonTask foundationMoveTask;

    public void handleEvent(RobotEvent e) {

    }

    public void init() {
        super.init();
        // drivetrain
        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setCanonicalMotorDirection();
        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1, TankMechanumControlScheme.MotorDirection.NONCANONICAL);
        driveTask = new TeleopDriveTask(this, MiyazakiCalibration.SPEED_LIMIT, scheme, frontLeft, frontRight, backLeft, backRight);

        // mechanisms
        leftArm = hardwareMap.servo.get("leftArm");
        rightArm = hardwareMap.servo.get("rightArm");
        leftStoneArm = hardwareMap.servo.get("leftStoneArm");
        rightStoneArm = hardwareMap.servo.get("rightStoneArm");
        stoneArm = hardwareMap.servo.get("stoneArm");
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
        stoneArms = ArmToggle.STOW_ARM;
        foundationArms = ArmToggle.STOW_ARM;
        vLift = hardwareMap.dcMotor.get("vLift");
        hLift = hardwareMap.crservo.get("hLift");
        leftIntake = hardwareMap.dcMotor.get("leftIntake");
        rightIntake = hardwareMap.dcMotor.get("rightIntake");
        tapeMeasurer = hardwareMap.dcMotor.get("tapePark");
        tapeMeasurer.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        tapeMeasurer.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        claw = hardwareMap.servo.get("claw");

        // sensors
        touchRight = hardwareMap.get(TouchSensor.class, "touchRight");
        touchLeft = hardwareMap.get(TouchSensor.class, "touchLeft");
        touchRightCriteria = new TouchSensorCriteria(touchRight);
        touchLeftCriteria = new TouchSensorCriteria(touchLeft);
    }

    @Override
    public void start() {
        this.addTask(driveTask);

        // GAMEPAD 2
        DeadmanMotorTask liftUp = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        addTask(liftUp);
        DeadmanMotorTask liftDown = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        addTask(liftDown);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_A_DOWN:
                        break;
                    case BUTTON_X_DOWN:
                        if (cMode == clawMode.CLAW_CLOSE) {
                            claw.setPosition(MiyazakiCalibration.NEW_CLAW_OPEN);
                            cMode = clawMode.CLAW_OPEN;
                        } else {
                            claw.setPosition(MiyazakiCalibration.NEW_CLAW_CLOSE);
                            cMode = clawMode.CLAW_CLOSE;
                        }
                        break;
                    case BUTTON_B_DOWN:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                        break;
                    case BUTTON_B_UP:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                        break;
                    case BUTTON_Y_DOWN:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                        break;
                    case BUTTON_Y_UP:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
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
                    case DPAD_UP_DOWN:
                        driveTask.suspend();
                        latchFoundation();
                        break;
                    case DPAD_RIGHT_DOWN:
                        driveTask.suspend();
                        drivetrain.strafe(0.2);
                        break;
                    case DPAD_RIGHT_UP:
                    case DPAD_LEFT_UP:
                        driveTask.resume();
                        drivetrain.strafe(0.0);
                        break;
                    case DPAD_LEFT_DOWN:
                        driveTask.suspend();
                        drivetrain.strafe(-0.2);
                        break;
                    case DPAD_DOWN_DOWN:
                        driveTask.suspend();
                        moveBack();
                        break;
                }

            }
        });

        // GAMEPAD 1
        /*
        DeadmanMotorTask tapeOut = new DeadmanMotorTask(this, tapeMeasurer, MiyazakiCalibration.TAPE_OUT, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(tapeOut);

        final DeadmanMotorTask tapeIn = new DeadmanMotorTask(this, tapeMeasurer, MiyazakiCalibration.TAPE_IN, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.BUTTON_B);
        addTask(tapeIn);

         */

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case RIGHT_TRIGGER_DOWN:
                        rightIntake.setPower(MiyazakiCalibration.INTAKE_RIGHT_COLLECT);
                        leftIntake.setPower(MiyazakiCalibration.INTAKE_LEFT_COLLECT);
                        claw.setPosition(MiyazakiCalibration.NEW_CLAW_OPEN);
                        break;
                    case LEFT_TRIGGER_DOWN:
                        rightIntake.setPower(MiyazakiCalibration.INTAKE_RIGHT_DISPENSE);
                        leftIntake.setPower(MiyazakiCalibration.INTAKE_LEFT_DISPENSE);
                        claw.setPosition(MiyazakiCalibration.NEW_CLAW_OPEN);
                        break;
                    case BUTTON_X_DOWN:
                        moveFoundationArms();
                        break;
                    case BUTTON_Y_DOWN:
                        if (dMode == drivetrainMode.SLOW_MODE) {
                            driveTask.slowDown(false);
                            dMode = drivetrainMode.FAST_MODE;
                        } else {
                            driveTask.slowDown(true);
                            dMode = drivetrainMode.SLOW_MODE;
                        }
                        driveTask.slowDown(false);
                        break;
                    case LEFT_BUMPER_DOWN:
                        leftIntake.setPower(0.0);
                        rightIntake.setPower(0.0);
                        intakeOn = false;
                        break;
                    case RIGHT_BUMPER_DOWN:
                        stoneArm.setPosition(MiyazakiCalibration.STONE_ARM_PUSH);
                        break;
                    case RIGHT_BUMPER_UP:
                        stoneArm.setPosition(MiyazakiCalibration.STONE_ARM_STOW);
                        break;
                    case BUTTON_A_DOWN:
                        tapeMeasurer.setPower(1.0);
                        break;
                    case BUTTON_B_DOWN:
                        tapeMeasurer.setPower(-1.0);
                        break;
                    case BUTTON_A_UP: case BUTTON_B_UP:
                        tapeMeasurer.setPower(0.0);
                        break;
                }
            }
        });
    }

    public void moveBack() {

        DeadReckonPath moveBackPath = new DeadReckonPath();
        moveBackPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, -0.6);
        DeadReckonTask moveBack = new DeadReckonTask(this, moveBackPath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        drivetrain.stop();
                        driveTask.resume();
                        break;
                }
            }
        };
        addTask(moveBack);
    }

    public void latchFoundation() {

        RobotLog.ii(TAG, "Latch Foundation");
        touchPath = new DeadReckonPath();
        touchPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.2);
        foundationMoveTask = new DeadReckonTask(this, touchPath, drivetrain, touchLeftCriteria, touchRightCriteria) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case BOTH_SENSORS_SATISFIED:
                        drivetrain.stop();
                        driveTask.resume();
                        // leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_DOWN);
                        // rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_DOWN);
                        foundationMoveTask.disableSensors();
                        break;
                }
            }
        };
        addTask(foundationMoveTask);
    }

    public void moveFoundationArms()
    {
        switch (foundationArms) {
            case DEPLOY_ARM:
                leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
                rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
                foundationArms = ArmToggle.STOW_ARM;
                break;
            case STOW_ARM:
                leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_DOWN);
                rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_DOWN);
                foundationArms = ArmToggle.DEPLOY_ARM;
                break;
        }
    }

    public void moveStoneArms ()
    {
        switch (stoneArms) {
            case DEPLOY_ARM:
                leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                stoneArms = ArmToggle.STOW_ARM;
                break;
            case STOW_ARM:
                leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
                stoneArms = ArmToggle.DEPLOY_ARM;
                break;
        }
    }
}
