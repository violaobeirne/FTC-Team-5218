package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.DigitalChannelController;
import com.qualcomm.robotcore.hardware.Servo;

import java.util.ServiceConfigurationError;

import team25core.ColorSensorTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Izzie on 2/10/2016.
 */
@Autonomous(name = "TEST Color", group = "TEST")
public class ColorSensorTest extends Robot {

    private DeviceInterfaceModule interfaceModule;
    // private ColorSensor colorLeft;
    private ColorSensor colorRight;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {
        interfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
        // colorLeft = hardwareMap.colorSensor.get("colorLeft");
        colorRight = hardwareMap.colorSensor.get("color");

        interfaceModule.setDigitalChannelMode(0, DigitalChannelController.Mode.OUTPUT);
        interfaceModule.setDigitalChannelState(0, false);
    }

    @Override
    public void loop() {

        // int leftRed = colorLeft.red();
        // int leftBlue = colorLeft.blue();
        int rightRed = colorRight.red();
        int rightBlue = colorRight.blue();

        /*
        if (leftRed > leftBlue) {
            telemetry.addData("STATE (L): ", "Red");
        } else if (leftRed < leftBlue) {
            telemetry.addData("STATE (L): ", "Blue");
        } else {
            telemetry.addData("STATE (L): ", "Unknown");
        }
        */

        if (rightRed > rightBlue) {
            telemetry.addData("STATE (R): ", "Red");
        } else if (rightRed < rightBlue) {
            telemetry.addData("STATE (R): ", "Blue");
        } else {
            telemetry.addData("STATE (R): ", "Unknown");
        }

        // telemetry.addData("LEFT (R): ", leftRed);
        // telemetry.addData("LEFT (B): ", leftBlue);

        telemetry.addData("RIGHT (R): ", rightRed);
        telemetry.addData("RIGHT (B): ", rightBlue);

    }
}
