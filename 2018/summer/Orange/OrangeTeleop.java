package summer;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.HisaishiCalibration;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 5/16/2018.
 */

public class GreenTeleop extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private FourWheelDirectDrivetrain fwd;
    private TankDriveTask driveTask;


    // add or remove servos/motors if you have any, make sure the servo name is appropriate so you can tell which one it is
    private Servo servoName;
    private DcMotor motorName;

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");

        servoName = hardwareMap.servo.get("servoOne");
        motorName = hardwareMap.dcMotor.get("motorOne");

        fwd = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        fwd.resetEncoders();
        fwd.encodersOn();

        driveTask = new TankDriveTask(this, fwd);
    }

    @Override
    public void start() {
        addTask(driveTask);
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                RobotLog.i("Gamepad Event", event.kind);
                if (event.kind == EventKind.BUTTON_A_DOWN) {

                } else if (event.kind == EventKind.BUTTON_B_DOWN) {

                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {

                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // .. nothing to see here
    }
}
