package opmodes;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMULevelSensorCriteria;
import team25core.MineralDetectionTask;
import team25core.MonitorMotorTask;
import team25core.OneWheelDirectDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "League Meet 3 Drop Autonomous")
public class LeagueMeet3BrentanoAutonomous extends Robot {

    private enum AllianceColor {
        BLUE,
        RED,
        DEFAULT
    }

    // declaring drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;

    // declaring hanging mechamisms and tasks
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftDone = false;
    private boolean rightDone = false;

    // declaring other tasks and paths
    private MineralMarkerDropoff dropoff;
    private DeadReckonPath exitDepotPath;
    private DeadReckonPath knockPath;

    // declaring gamepad variables
    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected MineralMarkerDropoff.DropMarker dropMarker;
    protected MineralMarkerDropoff.GoldMineralPosition goldMineralPosition;

    // declaring telemetry item
    private Telemetry.Item allianceItem;
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item dropMarkerItem;
    private Telemetry.Item landedItem;

    @Override
    public void init() {
        // initializing drivetrain
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();

        // hanging mechanism
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        // depot path
        exitDepotPath = new DeadReckonPath();
        exitDepotPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.2);
        knockPath = new DeadReckonPath();
        dropoff = new MineralMarkerDropoff();

        // initializing gamepad variables
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        // initializing telemetry items
        allianceItem = telemetry.addData("Alliance: ", "NOT SELECTED");
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        dropMarkerItem = telemetry.addData("Drop Marker", "NOT SELECTED");
        landedItem = telemetry.addData("Robot Landed", "Hanging around");

        // initializing mineral detection
        initializeMineralDetection();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                if (updatedMinerals != null) {
                    numberOfMineralsItem.setValue(updatedMinerals.size());
                    int goldMineralX = -1;
                    int silverMineral1X = -1;
                    int silverMineral2X = -1;
                    for (Recognition recognition : updatedMinerals) {
                        if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                            goldMineralX = (int) recognition.getLeft();
                        } else if (silverMineral1X == -1) {
                            silverMineral1X = (int) recognition.getLeft();
                        } else {
                            silverMineral2X = (int) recognition.getLeft();
                        }
                    }
                    if (goldMineralX != -1 && silverMineral1X != -1 && silverMineral2X != -1) {
                        if (goldMineralX < silverMineral1X && goldMineralX < silverMineral2X) {
                            goldMineralPositionItem.setValue("LEFT");
                            goldMineralPosition = MineralMarkerDropoff.GoldMineralPosition.LEFT;
                        } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                            goldMineralPositionItem.setValue("RIGHT");
                            goldMineralPosition = MineralMarkerDropoff.GoldMineralPosition.RIGHT;
                        } else {
                            goldMineralPositionItem.setValue("CENTER");
                            goldMineralPosition = MineralMarkerDropoff.GoldMineralPosition.CENTER;
                        }
                    }
                }

            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }


    public void liftUp()
    {
       addTask(new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
           public void handleEvent(RobotEvent e) {
               RunToEncoderValueEvent event = (RunToEncoderValueEvent) e;
               switch (event.kind) {
                   case DONE:
                       RobotLog.i("251: Left done");
                       liftLeft.setPower(VivaldiCalibration.LIFT_STOP);
                       leftDone = true;
               }
           }
       });

       addTask(new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
          public void handleEvent(RobotEvent e) {
              RunToEncoderValueEvent event = (RunToEncoderValueEvent) e;
              switch (event.kind) {
                  case DONE:
                      RobotLog.i("251: Right done");
                      liftRight.setPower(VivaldiCalibration.LIFT_STOP);
                      rightDone = true;
              }
          }
       });

       if(leftDone == true && rightDone == true) {
           RobotLog.i("251: It works!!!!!! Try initial move!!!! ");
           // initialMove(knockPath);
       }
    }

    @Override
    public void start() {
        knockPath = dropoff.getPath(dropMarker, goldMineralPosition);
        this.addTask(new MonitorMotorTask(this, liftLeft));
        this.addTask(new MonitorMotorTask(this, liftRight));
        liftUp();
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    allianceColor = AllianceColor.BLUE;
                    allianceItem.setValue("BLUE");
                    break;
                case BUTTON_B_DOWN:
                    allianceColor = AllianceColor.RED;
                    allianceItem.setValue("RED");
                    break;
                case RIGHT_BUMPER_DOWN:
                    dropMarker = MineralMarkerDropoff.DropMarker.TRUE;
                    dropMarkerItem.setValue("TRUE");
                    break;
                case LEFT_BUMPER_DOWN:
                    dropMarker = MineralMarkerDropoff.DropMarker.FALSE;
                    dropMarkerItem.setValue("FALSE");
                    break;
            }
        }
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                    markerDrop();
                }
            }
        });
    }

    protected void markerDrop() {
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                // drop marker rn
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                }
            }
        });
    }
}