package test;

import com.qualcomm.hardware.modernrobotics.ModernRoboticsI2cRangeSensor;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.UltrasonicSensor;

import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import team25core.GamepadTask;
import team25core.MonitorUltrasonicSensorTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Izzie on 2/20/2016.
 */
@Autonomous(name = "TEST MR Range Sensor", group = "TEST")
public class ModernRoboticsRangeSensorTest extends OpMode {
    private ModernRoboticsI2cRangeSensor sensor;

    @Override
    public void init() {

        sensor = hardwareMap.get(ModernRoboticsI2cRangeSensor.class, "rangeSensor");
    }

    @Override
    public void loop() {
        telemetry.addData("Distance", sensor.getDistance(DistanceUnit.CM));
    }

}
