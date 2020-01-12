package test;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DistanceSensor;

import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import opmodes.calibration.MiyazakiCalibration;
import team25core.GamepadTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/23/2019.
 */
@TeleOp(name = "H Lift Test")
public class HorizontalLiftTest extends Robot {

    private CRServo hLift;
    private DistanceSensor distanceSensor; // lowkey a V3 color sensor but a distance sensor in disguise
    private boolean hLiftStowed;

    @Override
    public void init() {
        hLift = hardwareMap.get(CRServo.class, "hLift");
        distanceSensor = hardwareMap.get(DistanceSensor.class, "distanceSensor");

        hLiftStowed = true;
    }

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_X_DOWN:
                        runHLift();
                }
            }
        });
    }

    public void runHLift() {
        if (hLiftStowed == true) {
            hLift.setPower(MiyazakiCalibration.HLIFT_OUT);
            if (distanceSensor.getDistance(DistanceUnit.INCH) <= MiyazakiCalibration.HLIFT_DIST) {
                hLift.setPower(0.0);
                hLiftStowed = false;
            }
        } else if (hLiftStowed == false) {
            hLift.setPower(MiyazakiCalibration.HLIFT_IN);
            if (distanceSensor.getDistance(DistanceUnit.INCH) >= MiyazakiCalibration.HLIFT_DIST) {
                hLift.setPower(0.0);
                hLiftStowed = true;
            }
        }
    }

}
