package test;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import team25core.IMULevelSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/2/2018.
 */
@Autonomous(name = "IMU Drop Test")
@Disabled
public class IMUDropTest extends Robot {
    // add scissor lift stuff to slowly move
    private IMULevelSensorCriteria levelSensorCriteria;
    private BNO055IMU imu;
    private Telemetry.Item landed;


    @Override
    public void init() {
        imu = hardwareMap.get(BNO055IMU.class, "imu");
        levelSensorCriteria = new IMULevelSensorCriteria(imu, 50.0);
        landed = telemetry.addData("LANDED: ", "FALSE");
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // keep scrolling...nothing to see here
    }

    protected void testPitch() {
        if (levelSensorCriteria.satisfied()) {
            landed.setValue("TRUE");
        }
    }

    @Override
    public void start() {
        testPitch();
    }
}
