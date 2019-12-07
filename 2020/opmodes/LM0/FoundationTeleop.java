package opmodes.LM0;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import opmodes.FoundationCalibration;
import opmodes.HisaishiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;

public class FoundationTeleop extends Robot {
        private DcMotor frontLeft;
        private DcMotor frontRight;
        private DcMotor backLeft;
        private DcMotor backRight;
        private MechanumGearedDrivetrain drivetrain;
        private TeleopDriveTask driveTask;
        private DcMotor lift;
        private Servo claw;
        private Servo leftArm;
        //private Servo rightArm;

        public void handleEvent (RobotEvent e) {

        }

        public void init()
        {
            frontLeft = hardwareMap.get(DcMotor.class, "frontLeft"); //drivetrain
            frontRight = hardwareMap.get(DcMotor.class, "frontRight");
            backLeft = hardwareMap.get(DcMotor.class, "backLeft");
            backRight = hardwareMap.get(DcMotor.class, "backRight");
            leftArm = hardwareMap.servo.get("leftArm");
            //rightArm = hardwareMap.servo.get("rightArm");

            TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1);
            drivetrain = new MechanumGearedDrivetrain(60, frontLeft,frontRight, backLeft, backRight);
            drivetrain.setNoncanonicalMotorDirection();
            driveTask = new TeleopDriveTask(this, scheme, frontLeft, frontRight, backLeft, backRight);
            lift = hardwareMap.dcMotor.get("lift");
            claw = hardwareMap.servo.get("claw");
        }

        @Override
        public void start()
        {
            this.addTask(driveTask);

            // GAMEPAD 2
            DeadmanMotorTask liftUp = new DeadmanMotorTask(this, lift, FoundationCalibration.LIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
            addTask(liftUp);
            DeadmanMotorTask liftDown = new DeadmanMotorTask(this, lift, HisaishiCalibration.LIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
            addTask(liftDown);

            this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
                public void handleEvent(RobotEvent e) {
                    GamepadEvent event = (GamepadEvent) e;
                    if (event.kind == EventKind.LEFT_BUMPER_DOWN) {
                        claw.setPosition(FoundationCalibration.OLD_CLAW_OPEN);
                    } else if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                        claw.setPosition(FoundationCalibration.OLD_CLAW_CLOSE);
                    //} else if (event.kind == EventKind.BUTTON_A_DOWN) {
                        //susan.setPosition(FoundationCalibration.SUSAN_OUT);
                    //} else if (event.kind == EventKind.BUTTON_B_DOWN) {
                       //susan.setPosition(FoundationCalibration.SUSAN_STOW);
                    } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                        leftArm.setPosition(FoundationCalibration.ARM_LEFT_STOW);
                        //rightArm.setPosition(HisaishiCalibration.ARM_RIGHT_STOW);
                    } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                        leftArm.setPosition(FoundationCalibration.ARM_LEFT_DOWN);
                        //rightArm.setPosition(HisaishiCalibration.ARM_RIGHT_DOWN);
                    }
                }
            });

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
        }
    }


