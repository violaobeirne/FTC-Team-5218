package opmodes.test;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.TouchSensor;

import team25core.DeadReckonPath;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import team25core.TeleopDriveTask;

@TeleOp(name = "REV TouchSensor Test")
public class REVTouchSensorTest extends Robot {
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private TeleopDriveTask driveTask;
    private MechanumGearedDrivetrain drivetrain;

    private DeadReckonPath TouchSensorTouchedPath;

    public void handleEvent(RobotEvent e) {

    }

    public void init() {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");
        touchRight = hardwareMap.get(TouchSensor.class, "touchRight");
        touchLeft = hardwareMap.get(TouchSensor.class, "touchLeft");

        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.encodersOn();
        drivetrain.resetEncoders();

        TouchSensorTouchedPath = new DeadReckonPath();

        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1, TankMechanumControlScheme.MotorDirection.NONCANONICAL);
        driveTask = new TeleopDriveTask(this, scheme, frontLeft, frontRight, backLeft, backRight);
    }
    @Override
    public void start() {
        this.addTask(driveTask);
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_X_DOWN:
                    TouchSensorTouchedPath();


                }
            }
        });
    }
}
