package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 10/20/2018.
 */
@TeleOp(name = "League Meet 2 Teleop")
@Disabled
public class LeagueMeet2ChopinTeleop extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor lift;
    private FourWheelDirectDrivetrain drivetrain;
    private TankDriveTask driveTask;


        @Override
        public void init() {
            frontLeft = hardwareMap.dcMotor.get("frontLeft");
            frontRight = hardwareMap.dcMotor.get("frontRight");
            backLeft = hardwareMap.dcMotor.get("backLeft");
            backRight = hardwareMap.dcMotor.get("backRight");
            lift = hardwareMap.dcMotor.get("lift");
            lift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
            lift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

            drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
            driveTask = new TankDriveTask(this, drivetrain);
        }
        @Override
        public void start() {
        driveTask = new TankDriveTask(this, drivetrain);
        addTask(driveTask);

        // DeadmanMotorTask raiseRobot = new DeadmanMotorTask(this, lift, VivaldiCalibration.LIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        // addTask(raiseRobot);

        // DeadmanMotorTask lowerRobot = new DeadmanMotorTask(this, lift, VivaldiCalibration.LIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        // addTask(lowerRobot);

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
                if (event.kind == EventKind.BUTTON_Y_DOWN) {

                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}

