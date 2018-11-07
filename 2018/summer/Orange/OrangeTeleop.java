package summer.Orange;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.HisaishiCalibration;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;
import team25core.TwoWheelDirectDrivetrain;
import team25core.TwoWheelDriveTask;

/**
 * Created by Lizzie on 5/16/2018.
 */

@TeleOp(name = "Traffic Cones Teleop")
public class OrangeTeleop extends Robot {

    private DcMotor left;
    private DcMotor right;

    private TwoWheelDirectDrivetrain fwd;
    private TwoWheelDriveTask driveTask;

    // add or remove servos/motors if you have any, make sure the servo name is appropriate so you can tell which one it is
    private CRServo flapper;


    @Override
    public void init() {
        left = hardwareMap.dcMotor.get("Left");
        right = hardwareMap.dcMotor.get("Right");
        flapper = hardwareMap.crservo.get("flapper");

        fwd = new TwoWheelDirectDrivetrain(right, left);
        fwd.resetEncoders();
        fwd.encodersOn();

        driveTask = new TwoWheelDriveTask(this, right, left);
        driveTask.slowDown(false);
    }

    @Override
    public void start() {
        addTask(driveTask);
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                RobotLog.i("Gamepad Event", event.kind);
                if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    flapper.setPower(-1.0);
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    flapper.setPower(1.0);
                } else if (event.kind == EventKind.RIGHT_TRIGGER_UP || event.kind == EventKind.RIGHT_BUMPER_UP)
                    flapper.setPower(0.0);
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // .. nothing to see here
    }
}
