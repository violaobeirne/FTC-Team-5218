package test;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;
import team25core.TwoWheelDirectDrivetrain;
import team25core.TwoWheelDriveTask;

/**
 * Created by Lizzie on 9/26/2017.
 */

public class CounterRotatePrototypeTest extends Robot{

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor intakeLeft;
    private DcMotor intakeRight;
    private TwoWheelDirectDrivetrain drivetrain;

        @Override
        public void init() {
            // drivetrain
            frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
            frontRight = hardwareMap.get(DcMotor.class, "frontRight");

            drivetrain = new TwoWheelDirectDrivetrain(frontRight, frontLeft);

            // intake mech
            intakeLeft = hardwareMap.dcMotor.get("lintake");
            intakeRight = hardwareMap.dcMotor.get("rintake");
        }

        @Override
        public void handleEvent(RobotEvent e) {

        }

        @Override
        public void start() {
            // Drivetrain
            this.addTask(new TwoWheelDriveTask(this, frontRight, frontLeft));

            // Intake mech
            this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
                public void handleEvent(RobotEvent e) {
                    GamepadEvent event = (GamepadEvent) e;

                    if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                        intakeRight.setPower(0.5);
                        intakeLeft.setPower(-0.5);

                    } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                        intakeRight.setPower(-0.5);
                        intakeLeft.setPower(0.5);

                    }
                }
            });
        }
    }
