package test;

import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.MochaCalibration;
import opmodes.VelocityVortexBeaconArms;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Elizabeth on 1/4/2017.
 */
public class MochaBeaconArmsTest extends Robot {
    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo beacon;

    private ColorSensor colorLeft;
    private ColorSensor colorRight;

    private VelocityVortexBeaconArms beaconArms;

    private final int LEFT_COLOR_PORT = MochaCalibration.LEFT_COLOR_PORT;
    private final int RIGHT_COLOR_PORT = MochaCalibration.RIGHT_COLOR_PORT;

    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;

    private final VelocityVortexBeaconArms.ServoType SERVO_TYPE = VelocityVortexBeaconArms.ServoType.CONTINUOUS;

    private DeviceInterfaceModule deviceInterfaceModule;

    private FourWheelDirectDriveDeadReckon alignColorSensorWithButton;

    @Override
    public void handleEvent(RobotEvent e) {

    }

    @Override
    public void init() {

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        beacon = hardwareMap.servo.get("beacon");

        colorRight = hardwareMap.colorSensor.get("colorRight");

        deviceInterfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
        beaconArms = new VelocityVortexBeaconArms(this, deviceInterfaceModule, alignColorSensorWithButton, beacon, SERVO_TYPE, true);

        alignColorSensorWithButton = new FourWheelDirectDriveDeadReckon
                (this, TICKS_PER_INCH, TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        alignColorSensorWithButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 2, 0.5 * -MOVE_SPEED);
    }

    @Override
    public void start() {
        addTask(new ColorSensorTask(this, colorRight, deviceInterfaceModule, false, true, RIGHT_COLOR_PORT) {
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
        });
    }
}
