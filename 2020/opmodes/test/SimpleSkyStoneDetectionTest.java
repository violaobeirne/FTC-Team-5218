package opmodes.test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DistanceSensor;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import team25core.Robot;
import team25core.RobotEvent;
import test.SkystoneDetectionTask;

@Autonomous(name="SimpleStoneDetectionTest")
public class SimpleSkyStoneDetectionTest extends Robot {

    ColorSensor colorSensor;
    DistanceSensor distanceSensor;
    Telemetry.Item stone;

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void init()
    {
        colorSensor = hardwareMap.get(ColorSensor.class, "leftColorSensor");
        distanceSensor = hardwareMap.get(DistanceSensor.class, "leftColorSensor");
        stone = telemetry.addData("Stone: ", "NO");
    }

    @Override
    public void start()
    {
        SkystoneDetectionTask skystoneDetectionTask = new SkystoneDetectionTask(this, colorSensor, distanceSensor) {
            public void handleEvent(RobotEvent e) {
                SkystoneDetectionTask.SkystoneDetectionEvent event = (SkystoneDetectionTask.SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        stone.setValue("DETECTED");
                        break;
                }
            }
        };
        addTask(skystoneDetectionTask);
    }
}
