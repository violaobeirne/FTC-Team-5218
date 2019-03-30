package test;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.hardware.rev.RevBlinkinLedDriver;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.Utilities.IMUUtil;
import opmodes.Utilities.VivaldiCalibration;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.MotionDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SimpleMonitorMotorTask;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 3/23/2019.
 */
@TeleOp(name = "Simple Motion Detection Test")
public class SimpleMotionDetectionTest extends Robot {
    // variables for drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    // variables for MotionDetection
    private TankDriveTask driveTask;
    private BNO055IMU imu;
    private RevBlinkinLedDriver blinkin;
    private GamepadTask gamepad1;

    @Override
    public void init() {
        // drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        // motion detection task and gamepad
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);

        // imu and lights
        blinkin = hardwareMap.get(RevBlinkinLedDriver.class, "blinkin");
        imu = IMUUtil.getIMU(hardwareMap);
    }

    @Override
    public void start() {
        driveTask = new TankDriveTask(this, drivetrain);
        addTask(driveTask);
        this.addTask(new SimpleMonitorMotorTask(this, frontLeft) {
            public void handleEvent (RobotEvent e) {
                SimpleMonitorMotorEvent event = (SimpleMonitorMotorEvent) e;
                if (event.kind == EventKind.MOVING_FORWARD || event.kind == EventKind.MOVING_BACKWARD) {
                    RobotLog.i("163: Robot moving.");
                    blinkin.setPattern(VivaldiCalibration.TELEOP_PATTERN);
                } else if (event.kind == EventKind.STOPPED) {
                    RobotLog.i("163: Robot stopped.");
                    blinkin.setPattern(VivaldiCalibration.ENDGAME_PATTERN);
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

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
