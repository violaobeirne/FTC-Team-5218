package test;

import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.ILT.BeethovenILTSkystonePath;
import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.StandardFourMotorRobot;
import team25core.TankMechanumControlScheme;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "5218 Skystone Detection Test")
public class SkystoneDetectionTest extends StandardFourMotorRobot {

    // drivetrain and mechanisms declaration
    private Servo leftStoneArm;
    private Servo rightStoneArm;
    private MechanumGearedDrivetrain drivetrain;

    // sensor
    private ColorSensor leftColorSensor;
    private DistanceSensor leftDistanceSensor;
    private ColorSensor rightColorSensor;
    private SkystoneDetectionTask skystoneDetectionTask;

    // gamepad and telemetry declaration
    private Telemetry.Item stone;

    // skybridge constant declaration
    private DeadReckonPath skystonePath;
    private BeethovenILTSkystonePath skystone;
    private BeethovenILTSkystonePath.AllianceColor allianceColor;

    @Override
    public void init () {
        // drivetrain and mechanisms initialization
        leftStoneArm = hardwareMap.servo.get("leftStoneArm");
        rightStoneArm = hardwareMap.servo.get("rightStoneArm");
        TankMechanumControlScheme scheme = new TankMechanumControlScheme(gamepad1, TankMechanumControlScheme.MotorDirection.NONCANONICAL);
        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        stone = telemetry.addData("Stone Position: ", "NOT SELECTED");

        // skystone and sensor initialization
        leftColorSensor = hardwareMap.get(RevColorSensorV3.class, "leftColorSensor");
        leftDistanceSensor = hardwareMap.get(RevColorSensorV3.class, "leftColorSensor");
        // rightColorSensor = hardwareMap.colorSensor.get("rightColorSensor");

        skystonePath = new DeadReckonPath();
        allianceColor = allianceColor.DEFAULT;

        // servo intiialization
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
    }

    @Override
    public void handleEvent (RobotEvent e) {
    }

    @Override
    public void start () {
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
        detectColor();
    }

    public void detectColor()
    {
        skystoneDetectionTask = new SkystoneDetectionTask(this, leftColorSensor, leftDistanceSensor) {
            public void handleEvent(RobotEvent e) {
                SkystoneDetectionTask.SkystoneDetectionEvent event = (SkystoneDetectionTask.SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        stone.setValue("DETECTED");
                        drivetrain.stop();
                        break;
                }
            }
        };
        this.addTask(skystoneDetectionTask);
    }
}
