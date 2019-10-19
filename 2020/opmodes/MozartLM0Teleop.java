package opmodes;

import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;


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
    private DcMotor frontLeft; //drivetrain
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private MechanumGearedDrivetrain drivetrain;
    private TeleopDriveTask driveTask;
    private DcMotor leftIntake; //wheel intake
    private DcMotor rightIntake;
    private DcMotor lift; //linear lift

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

        //leftIntake = hardwareMap.get(DcMotor.class, "leftIntake"); //wheel intake
        //rightIntake = hardwareMap.get(DcMotor.class, "rightIntake");
        //lift = hardwareMap.dcMotor.get("lift"); //linear lift

        GamepadTask gamepad= new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad);
    }

    @Override
    public void start()
    {
        this.addTask(driveTask);

        // GAMEPAD 1
        DeadmanMotorTask liftUp = new DeadmanMotorTask(this, lift, HisaishiCalibration.LIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        final DeadmanMotorTask liftDown = new DeadmanMotorTask(this, lift, HisaishiCalibration.LIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);

        /*
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {

                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {

                } else if (event.kind == EventKind.BUTTON_X_DOWN) {

                }
            }
        }

         */

    }
}
