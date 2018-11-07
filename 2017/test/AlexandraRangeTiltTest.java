package test;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.hardware.bosch.JustLoggingAccelerationIntegrator;
import com.qualcomm.hardware.modernrobotics.ModernRoboticsI2cRangeSensor;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.navigation.AngleUnit;
import org.firstinspires.ftc.robotcore.external.navigation.AxesOrder;
import org.firstinspires.ftc.robotcore.external.navigation.AxesReference;
import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;
import org.firstinspires.ftc.robotcore.external.navigation.Orientation;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.RangeSensorCriteria;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SensorCriteria;
import team25core.TwoWheelDirectDrivetrain;

/**
 * Created by Alexandra on 11/14/17.
 */

@Autonomous(name = "Alexandra: Range Test", group = "Team5218")
@Disabled

public class AlexandraRangeTiltTest extends Robot {

    public BNO055IMU imu;
    public AlexandraTiltCriteria tiltSensorCriteria;
    private DeadReckonPath path;
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private TwoWheelDirectDrivetrain drivetrain;

    @Override
    public void init() {
        //rangeSensor = hardwareMap.get(ModernRoboticsI2cRangeSensor.class, "range");
        //rangeSensorCriteria = new RangeSensorCriteria(rangeSensor, 15);

        tiltSensorCriteria = new AlexandraTiltCriteria(imu, 45);

        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        //rearLeft = hardwareMap.dcMotor.get("rearLeft");
        //rearRight = hardwareMap.dcMotor.get("rearRight");

        // Set up the parameters with which we will use our IMU. Note that integration
        // algorithm here just reports accelerations to the logcat log; it doesn't actually
        // provide positional information.
        BNO055IMU.Parameters parameters = new BNO055IMU.Parameters();
        parameters.angleUnit           = BNO055IMU.AngleUnit.DEGREES;
        parameters.accelUnit           = BNO055IMU.AccelUnit.METERS_PERSEC_PERSEC;
        parameters.calibrationDataFile = "BNO055IMUCalibration.json"; // see the calibration sample opmode
        parameters.loggingEnabled      = true;
        parameters.loggingTag          = "IMU";
        parameters.accelerationIntegrationAlgorithm = new JustLoggingAccelerationIntegrator();

        // Retrieve and initialize the IMU. We expect the IMU to be attached to an I2C port
        // on a Core Device Interface Module, configured to be a sensor of type "AdaFruit IMU",
        // and named "imu".
        imu = hardwareMap.get(BNO055IMU.class, "imu");
        imu.initialize(parameters);
        drivetrain = new TwoWheelDirectDrivetrain(frontRight, frontLeft);

        path = new DeadReckonPath();
        path.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, -0.3);
    }

    @Override
    public void start() {
        addTask(new DeadReckonTask(this, path, drivetrain, tiltSensorCriteria) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.SENSOR_SATISFIED) {
                    RobotLog.i("Max tilt reached", "stopping path");


                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e) {


    }
}

class AlexandraTiltCriteria implements SensorCriteria {
    private double max_tilt;

    BNO055IMU imu;

    public AlexandraTiltCriteria(BNO055IMU imu, double max_tilt) {
        this.imu = imu;
        this.max_tilt = max_tilt;
    }

    @Override
    public boolean satisfied() {

        Orientation angles = imu.getAngularOrientation(AxesReference.INTRINSIC, AxesOrder.ZYX, AngleUnit.DEGREES);

        double tilt = Math.toDegrees(Math.acos(Math.cos(Math.toRadians(angles.secondAngle))*
                Math.cos(Math.toRadians(angles.thirdAngle))));

       RobotLog.i("251 Tilt %f");

        if (tilt <= max_tilt) {
            return true;
        } else {
            return false;
        }
    }
}