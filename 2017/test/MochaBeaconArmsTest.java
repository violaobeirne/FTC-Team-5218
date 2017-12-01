package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.DigitalChannelController;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.MochaCalibration;
import opmodes.MochaParticleBeaconAutonomous;
import opmodes.VelocityVortexBeaconArms;
import team25core.ColorSensorTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Elizabeth on 1/4/2017.
 */

@Autonomous(name = "TEST Beacon", group = "TEST")
public class MochaBeaconArmsTest extends Robot {

    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo beacon;
    private DeviceInterfaceModule deviceInterfaceModule;
    private ColorSensor colorRight;

    private VelocityVortexBeaconArms beaconArms;

    private final int RIGHT_COLOR_PORT = 5;
    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;

    private FourWheelDirectDrivetrain drivetrain;
    private DeadReckonPath moveToNextButton;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        beacon = hardwareMap.servo.get("beacon");

        deviceInterfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
        deviceInterfaceModule.setDigitalChannelMode(0, DigitalChannelController.Mode.OUTPUT);
        deviceInterfaceModule.setDigitalChannelState(0, false);

        colorRight = hardwareMap.colorSensor.get("colorRight");

        moveToNextButton = new DeadReckonPath();
        moveToNextButton.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 3, 0.5 * -MOVE_SPEED);

        beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, moveToNextButton, drivetrain, beacon, true);

    }

    @Override
    public void start() {
        ColorSensorTask colorSensorTask = new ColorSensorTask(this, colorRight, deviceInterfaceModule, false,  RIGHT_COLOR_PORT) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent color = (ColorSensorEvent) e;

                switch (color.kind) {
                    case BLUE:
                        beaconArms.deploy(true, true);
                        break;
                    case RED:
                        beaconArms.deploy(false, true);
                        break;
                }
            }
        };
        colorSensorTask.setModeCompare(MochaCalibration.COLOR_THRESHOLD);
        addTask(colorSensorTask);
    }
}
