package opmodes.LM0;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import opmodes.HisaishiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;


@TeleOp(name = "5218 LM1 Teleop")
//@Disabled

public class BeethovenLM1Teleop extends Robot{
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
        private DcMotor lift;
        private Servo claw;
        private Servo susan;
        private Servo foundationMigration;

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
            drivetrain.setNoncanonicalMotorDirection();
            driveTask = new TeleopDriveTask(this, scheme, frontLeft, frontRight, backLeft, backRight);
            lift = hardwareMap.dcMotor.get("lift");
            claw = hardwareMap.servo.get("claw");
            susan = hardwareMap.servo.get("susan");
            foundationMigration = hardwareMap.servo.get("foundationMigration");
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
                    if (event.kind == EventKind.BUTTON_X_DOWN) {
                        claw.setPosition(HisaishiCalibration.NEW_CLAW_OPEN);
                    } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                        claw.setPosition(HisaishiCalibration.OLD_CLAW_CLOSE);
                    } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                        claw.setPosition(HisaishiCalibration.FOUNDATIOIN_MIGRATION_STOW);
                    } else if(event.kind == EventKind.BUTTON_Y_UP) {
                        foundationMigration.setPosition(HisaishiCalibration.FOUNDATION_MIGRATION_DOWN);
                    } else if (event.kind == EventKind.LEFT_BUMPER_DOWN) {
                        susan.setPosition(HisaishiCalibration.SUSAN_OUT);
                    } else if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                        susan.setPosition(HisaishiCalibration.SUSAN_STOW);
                   // } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                       // foundationMigration.setPosition(HisaishiCalibration.FOUNDATION_MIGRATION_DOWN);
                   // } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                      //  foundationMigration.setPosition(HisaishiCalibration.FOUNDATIOIN_MIGRATION_STOW);
                        // } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                        // leftArm.setPosition(HisaishiCalibration.ARM_LEFT_DOWN);
                        // rightArm.setPosition(HisaishiCalibration.ARM_RIGHT_DOWN);
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


