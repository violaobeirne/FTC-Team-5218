package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.HisaishiCalibration;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/9/2017.
 */
@Autonomous(name = "Jewel Arm Test")
public class JewelArmTest extends Robot {
    private Servo jewelXServo;
    private Servo jewelYServo;

    public static final double JEWEL_ARM_DEPLOY = HisaishiCalibration.JEWEL_Y_AXIS_DEPLOYED;
    public static final double JEWEL_ARM_STOW = HisaishiCalibration.JEWEL_Y_AXIS_STOWED;
    public static final double JEWEL_ARM_FORWARD = HisaishiCalibration.JEWEL_X_AXIS_FORWARD;
    public static final double JEWEL_ARM_BACK = HisaishiCalibration.JEWEL_X_AXIS_BACK;
    public static final double JEWEL_ARM_NEUTRAL = HisaishiCalibration.JEWEL_X_AXIS_NEUTRAL;

    public Telemetry.Item servoPosition;

    @Override
    public void init() {
        jewelXServo = hardwareMap.servo.get("jewelXAxis");
        jewelYServo = hardwareMap.servo.get("jewelYAxis");

        jewelXServo.setPosition(JEWEL_ARM_NEUTRAL);
        jewelYServo.setPosition(JEWEL_ARM_STOW);

        servoPosition = telemetry.addData("Servo Position: ", "neutral");
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // nothing to see here...continue on your scrolling adventures
    }

    @Override
    public void start() {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if(event.kind == EventKind.BUTTON_X_DOWN) {
                   jewelXServo.setPosition(JEWEL_ARM_BACK);
                   servoPosition.setValue("back");
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    jewelXServo.setPosition(JEWEL_ARM_FORWARD);
                    servoPosition.setValue("forward");
                } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    jewelYServo.setPosition(JEWEL_ARM_STOW);
                    servoPosition.setValue("stow");
                } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                    jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
                    servoPosition.setValue("deploy");
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    jewelXServo.setPosition(JEWEL_ARM_NEUTRAL);
                    servoPosition.setValue("neutral");
                }
            }
        });
    }
}
