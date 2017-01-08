package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.LightSensor;

/**
 * Created by Izzie on 2/17/2016.
 */
@Autonomous(name = "TEST Light", group = "TEST")
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
        telemetry.addData("Light (L) Raw: ", one.getRawLightDetected());
        telemetry.addData("Light (R) Raw: ", two.getRawLightDetected());

        telemetry.addData("Light (L): ", one.getLightDetected());
        telemetry.addData("Light (R): ", two.getLightDetected());

    }
}
