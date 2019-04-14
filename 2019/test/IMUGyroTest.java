package test;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.Utilities.IMUUtil;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

@Autonomous(name = "Gyro Test")
public class IMUGyroTest extends Robot {

    private final String TAG = "Gyro Test";
    Telemetry.Item gyroHeadingItem;
    BNO055IMU imu;
    IMUUtil imuUtil = new IMUUtil();

    @Override
    public void init()
    {
        imu = imuUtil.getIMU(hardwareMap);
        gyroHeadingItem = telemetry.addData("Heading ", 0);
    }

    @Override
    public void start()
    {
        GamepadTask gamepadTask = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            @Override
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent)e;
                switch (event.kind) {
                    case BUTTON_A_UP:
                        RobotLog.ii(TAG, "Reseting heading");
                        imuUtil.resetHeading();
                    default:
                }
            }
        };
        addTask(gamepadTask);
    }

    @Override
    public void loop()
    {
        super.loop();

        gyroHeadingItem.setValue(imuUtil.getHeadingInDegrees(imu));
    }

    @Override
    public void handleEvent(RobotEvent e) { }
}
