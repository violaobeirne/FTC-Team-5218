package opmodes.test;

import android.content.Context;
import android.util.Log;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.util.ElapsedTime;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.internal.system.AppUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

@Autonomous(name="ColorSensorTest")
public class ColorSensorTest extends OpMode {

    ColorSensor colorSensor;
    File file;
    FileOutputStream stream;
    ElapsedTime timer;
    Telemetry.Item status;

    @Override
    public void init()
    {
        Context context = AppUtil.getDefContext();
        try {
            File path = context.getExternalFilesDir(null);
            file = new File(path, "sensor_data.txt");
            stream = new FileOutputStream(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        colorSensor = hardwareMap.get(ColorSensor.class, "leftColorSensor");
        status = telemetry.addData("Status: ", "INITIALIZED");
    }

    @Override
    public void start()
    {
        timer = new ElapsedTime(ElapsedTime.Resolution.SECONDS);
        status.setValue("RUNNING");
    }

    @Override
    public void loop()
    {
        if (timer.time() > 10) {
            status.setValue("DONE");
            try {
                stream.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            return;
        }

        try {
            int red = colorSensor.red();
            int green = colorSensor.green();
            int blue = colorSensor.blue();

            stream.write(String.format("%d,%d,%d\n", red, green, blue).getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
