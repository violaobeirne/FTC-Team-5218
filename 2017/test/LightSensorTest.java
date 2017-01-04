package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.LightSensor;

/**
 * Created by Izzie on 2/17/2016.
 */
@Autonomous(name = "TEST Light")
public class LightSensorTest extends OpMode{

    LightSensor one;
    LightSensor two;

    @Override
    public void init()
    {
        one = hardwareMap.lightSensor.get("lightLeft");
        one.enableLed(true);
        two = hardwareMap.lightSensor.get("lightRight");
        two.enableLed(true);
    }

    @Override
    public void loop()
    {
        telemetry.addData("Light (1) Raw: ", one.getRawLightDetected());
        telemetry.addData("Light (2) Raw: ", two.getRawLightDetected());

        telemetry.addData("Light (1) :", one.getLightDetected());
        telemetry.addData("Light (2) :", one.getLightDetected());

    }
}
