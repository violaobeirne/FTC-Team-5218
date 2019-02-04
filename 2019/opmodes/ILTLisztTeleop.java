package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 10/20/2018.
 */
@TeleOp(name = "ILT Teleop")
public class ILTLisztTeleop extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor fourBar;
    private DcMotor bungeeBox;
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private Servo marker;
    private FourWheelDirectDrivetrain drivetrain;
    private TankDriveTask driveTask;


    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        fourBar = hardwareMap.dcMotor.get("fourBar");
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        marker = hardwareMap.servo.get("marker");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);
        fourBar.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        fourBar.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        fourBar.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);
        marker.setPosition(VivaldiCalibration.MARKER_STOWED);
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
    }

    public void liftUp() {
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftLeft.setPower(0.0);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftRight.setPower(0.0);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void liftDown() {
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_DOWN) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftLeft.setPower(0.0);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_DOWN) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftRight.setPower(0.0);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    @Override
    public void start() {
        driveTask = new TankDriveTask(this, drivetrain);
        addTask(driveTask);
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    driveTask.slowDown(false);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    driveTask.slowDown(true);
                }
            }
        });

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    liftDown();
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    liftUp();
                } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                    liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_DOWN);
                    liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_DOWN);
                } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_UP);
                    liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_UP);
                } else if (event.kind == EventKind.BUTTON_X_UP || event.kind == EventKind.BUTTON_Y_UP) {
                    liftLeft.setPower(VivaldiCalibration.LIFT_STOP);
                    liftRight.setPower(VivaldiCalibration.LIFT_STOP);
                }
            }
        });

        // Four
        DeadmanMotorTask raiseFourBar = new DeadmanMotorTask(this, fourBar, VivaldiCalibration.FOUR_BAR_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        addTask(raiseFourBar);

        DeadmanMotorTask lowerFourBar = new DeadmanMotorTask(this, fourBar, VivaldiCalibration.FOUR_BAR_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        addTask(lowerFourBar);

        DeadmanMotorTask deployBungeeBox = new DeadmanMotorTask(this, bungeeBox, VivaldiCalibration.BUNGEE_BOX_DEPLOY, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A);
        addTask(deployBungeeBox);

        DeadmanMotorTask stowBungeeBox = new DeadmanMotorTask(this, bungeeBox, VivaldiCalibration.BUNGEE_BOX_STOW, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_Y);
        addTask(stowBungeeBox);
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}

